#	MiniDspShd
#
#	----------------------------------------------------------------------
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#	02111-1307 USA
#
package Plugins::MiniDspShd::Plugin;
use strict;
use base qw(Slim::Plugin::Base);

use Slim::Utils::Strings qw(string);
use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Misc;
use Slim::Utils::Network;
use Slim::Networking::SimpleAsyncHTTP;
use JSON::XS::VersionOneAndTwo;
use HTTP::Status qw(RC_NOT_FOUND RC_OK);

#require Data::Dump;

use File::Spec::Functions qw(:ALL);
use FindBin qw($Bin);

use Plugins::MiniDspShd::Settings;

# Refresh our POST subscription on the shd every
# two minutes, in case it restarts and loses us.
use constant SHD_POST_SUB_REFRESH_INTERVAL => 120;

use constant SHD_API_V1_PUSH_NOTIFICATION_URLS => "/api/v1/pushNotificationUrls";
use constant SHD_PLUGIN_INITIATED => "minidspshdInitiated";

# ----------------------------------------------------------------------------
# Global variables
# ----------------------------------------------------------------------------
my $gFirstPlayerReported = 0;
my $gOrigVolCmdFuncRef;
#my $gOrigGetExternalVolumeInfoFuncRef;

# The regex here is intended to match typical mac address format, which
# is what is currently used for client->id.
my $MINIDSPSHD_POST_URL_PARSER_RE = qr{plugins/minidspshd/poststate/([a-f0-9:]+)}i;
use constant SHD_PLUGIN_POST_PATH => "/plugins/minidspshd/poststate/";

# Hash of clients currently active (inited) in this plugin.
# Indexed by client id, stores maxVol and shdAddress prefs,
# currVol, and mute state.
my %gActiveClients;

# The plugin must be restarted for settings changes to take affect. We could
# probably provide support otherwise, but in keeping things simple, we don't.
#
# This hash of clients that were disabled when first discovered ensures we will
# not activate support for them here in the event the user enables without
# a restart.  Sounds silly, but we want to be consistent and neither act on
# an enable or disable (or any other plugin pref change) until restart.
my %gDisabledClients;

# ----------------------------------------------------------------------------
my $log = Slim::Utils::Log->addLogCategory(
    {
	'category'     => 'plugin.minidspshd',
	'defaultLevel' => 'ERROR',
        'description'  => getDisplayName(),       
        'helper'       => '',                                 
    }
);

# ----------------------------------------------------------------------------
sub getDisplayName {
	return 'PLUGIN_MINIDSPSHD';
}

# ----------------------------------------------------------------------------
# This is called by the server once at startup.
# ----------------------------------------------------------------------------
sub initPlugin {
	my $classPlugin = shift;

	# FYI - log calls only work after this returns.

	# Re-enable this if we add additional functionality requiring menus
	# $classPlugin->SUPER::initPlugin();

	# Initialize settings classes
	my $classSettings = Plugins::MiniDspShd::Settings->new($classPlugin);

        # Reroute all mixer volume requests to us.
        $gOrigVolCmdFuncRef = Slim::Control::Request::addDispatch(['mixer', 'volume', '_newvalue'],
        							  [1, 0, 0, \&myMixerVolumeCommand]);
	# Expose our capability to the CLI.
	# FYI: disabling this, bug in the server causes
	# a crash (documented in forum, not yet resolved).
	# $gOrigGetExternalVolumeInfoFuncRef = Slim::Control::Request::addDispatch(['getexternalvolumeinfo'],
        #								         [0, 0, 0, \&getexternalvolumeinfoCLI]);

	# Subscribe to player discovery notifications.
	Slim::Control::Request::subscribe(\&newPlayerCheck, [['client']],[['new']]);
}

# ----------------------------------------------------------------------------
# The server calls this once at server shutdown.
# ----------------------------------------------------------------------------
sub shutdownPlugin {

	# Unsubscribe from all callbacks for all clients.
	Slim::Control::Request::unsubscribe(\&newPlayerCheck);

        # Restore original server functions we've re-routed to us.
        Slim::Control::Request::addDispatch(['mixer', 'volume', '_newvalue'], [1, 0, 0, $gOrigVolCmdFuncRef]);
        #Slim::Control::Request::addDispatch(['getexternalvolumeinfo'], [0, 0, 0, $gOrigGetExternalVolumeInfoFuncRef]);

	# Kill timer and unsubscribe for POST events from all active client SHD's.
	foreach my $clientId (keys %gActiveClients) {
		my $client = Slim::Player::Client::getClient($clientId);
		Slim::Utils::Timers::killTimers($client, \&sendShdSubscribe);

		# FYI: The unsubscribe request we're trying to send to the shd here
		# does not actually get sent (as tested with LMS 8.2.1), likely due
		# to the server shutting down.  We'll leave the code in place but
		# commented out, since it can't be fully tested in this context.

		# my $shdAddr = $gActiveClients{$client->id()}{shdAddr};
		# sendShdUnsubscribe($client, $shdAddr);
	}
}

# ----------------------------------------------------------------------------
# FYI: The server calls this not only when a player is discovered, it can
# also be called when prefs are changed, though that doesn't seem to be the
# norm.  We protect the plugin's internal state by ignoring this call except
# for the first time, regardless of user pref changes.
# ----------------------------------------------------------------------------
sub newPlayerCheck {
	my $request = shift;
	my $client = $request->client();

	if (!$gFirstPlayerReported) {
		# Display the plugin version in the log file
		# This is here instead of in initPlugin because
		# our log calls do not output until after initPlugin
		# returns.
		my $xmlname = catdir(Slim::Utils::PluginManager->allPlugins->{'MiniDspShd'}->{'basedir'}, 'install.xml');
		my $xml = new XML::Simple;
		my $data = $xml->XMLin($xmlname);
		$log->info("Version " . $data->{version});

		$gFirstPlayerReported = 1;
	}

    	if (!defined($client)) {
		$log->debug("missing client");
		return;
	}
	#Data::Dump::dump($client);

	$log->debug("client " . $client->name . " id: " . $client->id());

	# Don't limit device type, simply because I don't know
	# each device type's limitations. We're not directly
	# interacting with the player, so it shouldn't matter
	# to this plugin.
	#if(!(($client->isa("Slim::Player::Receiver")) || ($client->isa("Slim::Player::Squeezebox2")))) {
	#	$log->debug("not a receiver or a squeezebox b");
	#	return;
	#}

	# If we've already seen this client in disabled or
	# enabled state ignore it.  We want to force server
	# restart for user pref changes.  (see comment for
	# gDisabledClients).
	if (exists $gDisabledClients{$client->id()} ||
			exists $gActiveClients{$client->id()}) {
		$log->debug("client: " . $client->name . " already processed, restart required if user prefs change");
		return;
	}

        my $prefs = preferences('plugin.minidspshd');
	my $cprefs = $prefs->client($client);
	my $pref_Enabled = $cprefs->get('pref_Enabled');
	
	if (!$pref_Enabled) {
		$log->debug("client: " . $client->name . "plugin not enabled");
		# Add to list of disabled clients.
		$gDisabledClients{$client->id()} = 1;
		return;
	}

	initClient($client);
}

# ----------------------------------------------------------------------------
sub initClient {
	my $client = shift;

	# Add to our hash of active clients.
        my $prefs = preferences('plugin.minidspshd');
	my $cprefs = $prefs->client($client);
	my $maxVol = $cprefs->get('maxVol');
	my $shdAddr = $cprefs->get('shdAddress');
	$gActiveClients{$client->id()} = { maxVol => $maxVol, shdAddr => $shdAddr, currVol => 0, mute => 0 };

	$log->debug("plugin enabled for ip address " . $shdAddr . " maxVol: " . $maxVol);

	# Subscribe to client state changes.
	# XXX: Not needed for volume support, maybe later if we add other capabilities to the plugin.
	# Slim::Control::Request::subscribe(\&commandCallback, [['power', 'play', 'playlist', 'pause', 'client']], $client);

	# Request the current state from the SHD. This will update the client with current
	# volume and mute setting.
	sendShdGetState($client);

	# Register a handler and subscribe to POST events from the SHD.
       	Slim::Web::Pages->addRawFunction($MINIDSPSHD_POST_URL_PARSER_RE, \&_miniDspShdPostHandler);      
	sendShdSubscribe($client, $shdAddr);
}	

# ----------------------------------------------------------------------------
sub sendShdSubscribe {
	my $client = shift;
	my $shdAddr = shift;

	my $serverUrl = Slim::Utils::Network::serverURL();
	my $url = "http://" . $shdAddr . SHD_API_V1_PUSH_NOTIFICATION_URLS;
	my $postBody = "url=" . $serverUrl . SHD_PLUGIN_POST_PATH . $client->id();

	$log->debug("sending subscribe to " . $client->name . " url: " . $url . " body: " . $postBody);

       	my $http = Slim::Networking::SimpleAsyncHTTP->new(                                                         
               	\&_subscribeShdOK,                                                                                    
               	\&_subscribeShdError,                                                                                
               	{                                                                                               
			client  => $client,
                       	timeout => 10,
                       	error   => "Can't subscribe to SHD post",                                                    
               	},                                                                                         
       	);                                                                                                 
       	$http->post($url, 'Content-Type' => 'application/x-www-form-urlencoded', $postBody);

        # Continue to send these periodically. This way, if the shd restarts and
	# drops our subscription we will refresh it.
	Slim::Utils::Timers::setTimer($client, (Time::HiRes::time() + SHD_POST_SUB_REFRESH_INTERVAL),
                                      \&sendShdSubscribe, $shdAddr);
}

# ----------------------------------------------------------------------------
sub _subscribeShdOK {
        my $http = shift;
	my $client = $http->params('client');

        #$log->debug("ok http callback for client: " . $client->name);

	return 0;
}

# ----------------------------------------------------------------------------
sub _subscribeShdError {
        my $http = shift;
        my $error = $http->params('error');
        #$log->error($error || "http error, no response.");
}

# ----------------------------------------------------------------------------
#sub sendShdUnsubscribe {
#	my $client = shift;
#	my $shdAddr = shift;
#
#	my $serverUrl = Slim::Utils::Network::serverURL();
#	my $url = "http://" . $shdAddr . SHD_API_V1_PUSH_NOTIFICATION_URLS;
#	my $body = "url=" . $serverUrl . SHD_PLUGIN_POST_PATH . $client->id();
#
#	$log->debug("sending unsubscribe to " . $client->name . " url: " . $url . " body: " . $body);
#
#	# SimpleAsyncHTTP doesn't support DELETE, so we use non-Simple.
#	my $header = [ 'Content-Type' => 'application/x-www-form-urlencoded'];
#	my $request = HTTP::Request->new('DELETE', $url, $header, $body);
#	my $http = Slim::Networking::Async::HTTP->new;
#        $http->send_request( {
#        	request     => $request,
#        	onError     => sub {
#        			my ($self, $error) = @_;
#        			$log->warn("could not send shd unsub $error" );
#        		},
#        } );
#}

# ----------------------------------------------------------------------------
sub _miniDspShdPostHandler {                                                           
    	my $httpClient = shift;
    	my $response = shift;

    	my $request = $response->request;                                         
	my $path = $request->uri->path;
	$log->debug("rcvd SHD POST event, path: " . $path);

	$response->code(RC_OK);                                                                                     

	# Identify the client from the id in the POST url.
	$path =~ $MINIDSPSHD_POST_URL_PARSER_RE;
	my $clientId = $1;
	my $client = Slim::Player::Client::getClient($clientId);
	if (!defined($client)) {
                $log->debug("failed to find client identified in SHD POST url, clientId: " . $clientId);
		return;
	}

	# Make sure the client is activated in this plugin.
	if (!exists $gActiveClients{$client->id()}) {
		$log->debug("received SHD POST for inactive client: " . $client->name());
		return;
	}

        my $infoRef = eval { from_json($request->content) };
	if ($@) {
                $log->error("failed to parse SHD state info: " . $@ . " for client: " . client->name);
		return;
	}
	if (!$infoRef) {
                $log->error("info not found in SHD post for client: " . client->name);
		return;
	}
	my $postedVol = $infoRef->{data}{volume};
	if ($@) {
                $log->error("volume not found in SHD post for client: " . client->name);
		return;
	}
	if ($postedVol < 0 || $postedVol > 100) {
                $log->error("received invalid volume: " . $postedVol . " from client: " . client->name);
		return;
	}

	my $postedMute = $infoRef->{data}{mute};
	if ($@) {
                $log->error("mute not found in SHD post for client: " . client->name);
		return;
	}
	$log->debug("rcvd SHD volume: " . $postedVol . " mute: " . $postedMute . " for client: " . $client->name);

    	# Update volume and mute in the client.
	updateSqueezeVolAndMute($client, $postedVol, $postedMute);
}

# ----------------------------------------------------------------------------
# All mixer changes are routed here.  We'll act on those we're enabled for
# and call the original (re-routed) function otherwise.
# ----------------------------------------------------------------------------
sub myMixerVolumeCommand {
	my $request = shift;
	my $client = $request->client();

	if(!defined($client)) {
		$log->debug("client not defined");
		return;
	}

	if (!exists $gActiveClients{$client->id()}) {
		$log->debug("received command for inactive client: " . $client->name());
		# Call original function.
		eval { &{$gOrigVolCmdFuncRef}($request) };
		return;
	}

	$log->debug("p0: " . $request->{'_request'}[0] . " p1: " . $request->{'_request'}[1]);

	# If this plugin made the request ignore it.
	my $selfInitiated = $request->getParam('_p3');

	if ($selfInitiated eq SHD_PLUGIN_INITIATED) {
		$log->debug("selfInitiated trapped");
       		$request->deleteParam('_p3');
		# Call original function.
		eval { &{$gOrigVolCmdFuncRef}($request) };
		return;
	}

	if ($request->isCommand([['mixer'], ['volume']])) {

		my $reqValue = $request->getParam('_newvalue');
		$log->debug("vol change requested: $reqValue");

		my $absValue = $reqValue;
		
		# If it's an incremental adjustment, get the current volume from our cache.
		my $char1 = substr($reqValue, 0, 1);
		if (($char1 eq '-') || ($char1 eq '+')) {
			my $currVol = $gActiveClients{$client->id()}{currVol};
			$absValue = $currVol + $reqValue;
		}

		# If the volume is already at the requested value, ignore.
		if ($gActiveClients{$client->id()}{currVol} == $absValue) {
			$log->debug("no change in resulting volume, ignore");
			# Call original function.
			eval { &{$gOrigVolCmdFuncRef}($request) };
			return;
		}

		my $shdVol = $absValue;

		# Don't send vol less than 0 to SHD.
		if ($shdVol < 0) {
			$shdVol = 0;
		}

		# Don't send vol greater than maxVol to SHD.
		my $maxVol = $gActiveClients{$client->id()}{maxVol};
		if ($shdVol > $maxVol) {
			$log->debug("requested vol: " . $shdVol . " exceeds maxVol: " . $maxVol . " reducing");
			$shdVol = $maxVol;
		}

		# Update the request structure with the
		# new absolute volume.
       		$request->deleteParam('_newvalue');
       		$request->addParam('_newvalue', $shdVol);                                                                                     
		my $currVol = $gActiveClients{$client->id}{currVol};
		if ($shdVol != $currVol ) {
			$gActiveClients{$client->id}{currVol} = $shdVol;
			sendShdSetVol($client, $shdVol);
		}

	} elsif ($request->isCommand([['mixer'], ['muting']])) {

		my $muteOn != $gActiveClients{$client->id()}{mute};

		$log->debug("muting toggle request: " . $muteOn);
		sendShdMuteToggle($client, $muteOn);
	}			

        # Call original function in the server.  If we
	# don't do this the volume info in the UI doesn't
	# get updated since the server doesn't know what
	# happened.
        eval { & { $gOrigVolCmdFuncRef} ($request) };
}

# ----------------------------------------------------------------------------
# Update the client volume and mute with the that requested from or posted by the SHD
#
sub updateSqueezeVolAndMute {
	my $client = shift;
	my $shdVol = shift;
	my $shdMute = shift;

	if ($shdVol < 0 || $shdVol > 100) {
		$log->debug("invalid SHD vol received from client: " . $client->name . " value: " . $shdVol);
		return;
	}

	# Update the client. Add a param so we can detect our
	# own adjustments and ignore them in myMixerVolumeCommand.

	if ($gActiveClients{$client->id()}{currVol} != $shdVol) {
		# Update our cached volume.
		$gActiveClients{$client->id()}{currVol} = $shdVol;

		$client->execute([('mixer', 'volume', $shdVol, SHD_PLUGIN_INITIATED)]);	
	} else {
		$log->debug("no vol change");
	}

	if ($gActiveClients{$client->id()}{mute} != $shdMute) {
		# Update our cached mute.
		$gActiveClients{$client->id()}{mute} = $shdMute;

		$client->execute([('mixer', 'muting', $shdMute, SHD_PLUGIN_INITIATED)]);	
	} else {
		$log->debug("no mute change");
	}
}

# ----------------------------------------------------------------------------
# External volume indication support code.
# Used by iPeng and possibly other controllers.
#
# FYI - when this is called (e.g. when ipeng is used) a crash
# occurs in the server.  This crash is documented on the forum
# but no resolution has yet to be identified.
#
#sub getexternalvolumeinfoCLI {
#	my @args = @_;
#
#	&reportOnOurPlayers();
#	if (defined($gOrigGetExternalVolumeInfoFuncRef) ) {
#		# chain to the next implementation
#		return &$gOrigGetExternalVolumeInfoFuncRef(@args);
#	}
#	# else we're authoritative
#	my $request = $args[0];
#	$request->setStatusDone();
#}
#
## ----------------------------------------------------------------------------
#sub reportOnOurPlayers() {
#	# loop through all currently attached players
#	foreach my $client (Slim::Player::Client::clients()) {
#		if (exists $gActiveClients{$client->id()}) {
#			# Using our volume control, report on our capabilities.
#			$log->debug($client->name . " uses this plugin for external volume control");
#			# FYI 0,1 are relative,precise
#			Slim::Control::Request::notifyFromArray($client, ['getexternalvolumeinfo', 0, 1, string(&getDisplayName())]);
#		}
#	}
#}
	
# ----------------------------------------------------------------------------
sub sendShdGetState {
	my $client = shift;

	my $shdAddr = $gActiveClients{$client->id()}{shdAddr};
	my $url = "http://" . $shdAddr . "/api/v1/getState";
                                                                                                           
	$log->debug("sending SHD getState for " . $client->name . "url: " . $url);

        my $http = Slim::Networking::SimpleAsyncHTTP->new(                                                         
                \&_getStateOK,                                                                                    
                \&_getStateError,                                                                                
                {                                                                                               
			client  => $client,
                        timeout => 10,
                        error   => "Can't get SHD volume",                                                    
                },                                                                                         
        );                                                                                                 

        $http->get($url);
}

sub _getStateOK {
        my $http = shift;
	my $client = $http->params('client');

        $log->debug("ok http getState callback for client: " . $client->name);

        my $resultRef = eval { from_json($http->content) };
	if ($@) {
        	$log->debug("error parsing SHD state", $@);
		return 1;
	}
	if (!$resultRef) {
        	$log->debug("bad result parsing SHD state", $@);
		return 1;
	}

	my %result = %$resultRef;
	my $returnedVol = $result{"volume"};
	my $returnedMute = $result{"mute"};

	$log->debug("SHD returned volume: " . $returnedVol . " mute: " . $returnedMute . " for client: " . $client->name);
                                                                                                                
    	# Update volume and mute in the client.
	updateSqueezeVolAndMute($client, $returnedVol, $returnedMute);

	return 0;
}

sub _getStateError {
        my $http = shift;
        my $error = $http->params('error');
                                                                                                                
        $log->error($error || "http error, no response.");
}

# ----------------------------------------------------------------------------
sub sendShdMuteToggle {
	my $client = shift;
	my $muteOn = shift;

	my $muteCmd;
	if ($muteOn) {
		$muteCmd = "mute";
	} else {
		$muteCmd = "unmute";
	}

	my $shdAddr = $gActiveClients{$client->id()}{shdAddr};
	my $url = "http://" . $shdAddr . "/api/v1/commands/?cmd=volume&volume=" . $muteCmd;
                                                                                                           
	$log->debug("sending " . $muteCmd . " to " . $client->name . "url: " . $url);

        my $http = Slim::Networking::SimpleAsyncHTTP->new(                                                         
                \&_muteToggleOK,                                                                                    
                \&_muteToggleError,                                                                                
                {                                                                                               
			client  => $client,
                        timeout => 10,                                                                     
                        error   => "Can't mute/unmute SHD volume",                                                    
                },                                                                                         
        );                                                                                                 

        $http->get($url);
}

sub _muteToggleOK {
        my $http = shift;
	my $client = $http->params('client');

        $log->debug("ok http callback for muteToggle for client: " . $client->name);

	return 0;
}

sub _muteToggleError {
        my $http = shift;
        my $error = $http->params('error');
                                                                                                                
        $log->error($error || "http error, no response.");
}

# ----------------------------------------------------------------------------
sub sendShdSetVol {
	my $client = shift;
	my $vol = shift;

	my $shdAddr = $gActiveClients{$client->id()}{shdAddr};
	my $url = "http://" . $shdAddr . "/api/v1/commands/?cmd=volume&volume=" . $vol;
                                                                                                           
	$log->debug("sending setVol to " . $client->name . "url: " . $url . " vol value: " . $vol);

        my $http = Slim::Networking::SimpleAsyncHTTP->new(                                                         
                \&_setVolOK,                                                                                    
                \&_setVolError,                                                                                
                {                                                                                               
			client  => $client,
                        timeout => 10,
                        error   => "Can't set SHD volume",                                                    
                },                                                                                         
        );                                                                                                 

        $http->get($url);
}

sub _setVolOK {
        my $http = shift;
	my $client = $http->params('client');

        $log->debug("ok http callback for setVol for client: " . $client->name);

	return 0;
}

sub _setVolError {
        my $http = shift;
        my $error = $http->params('error');
                                                                                                                
        $log->error($error || "http error, no response.");
}

# end 
1;
