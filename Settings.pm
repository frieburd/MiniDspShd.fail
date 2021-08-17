package Plugins::MiniDspShd::Settings;

# SqueezeCenter Copyright (c) 2001-2009 Logitech.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License, 
# version 2.

use strict;
use base qw(Slim::Web::Settings);

use Slim::Utils::Strings qw(string);
use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Scalar::Util qw(looks_like_number);

# ----------------------------------------------------------------------------
# Global variables
# ----------------------------------------------------------------------------
my $prefs = preferences('plugin.minidspshd');

my $log = Slim::Utils::Log->addLogCategory({
	'category'     => 'plugin.minidspshd',
	'defaultLevel' => 'DEBUG',
	'description'  => 'PLUGIN_MINIDSPSHD_MODULE_NAME',
});

# ----------------------------------------------------------------------------
# Name in the settings dropdown
# ----------------------------------------------------------------------------
sub name {
	return 'PLUGIN_MINIDSPSHD_MODULE_NAME';
}

# ----------------------------------------------------------------------------
# Webpage served for settings
# ----------------------------------------------------------------------------
sub page {
	return 'plugins/MiniDspShd/settings/basic.html';
}

# ----------------------------------------------------------------------------
# Settings are per player
# ----------------------------------------------------------------------------
sub needsClient {
	return 1;
}

# ----------------------------------------------------------------------------
# Only show plugin for Squeezebox 3 or Receiver players
# ----------------------------------------------------------------------------
sub validFor {
	my $class = shift;
	my $client = shift;

	return $client->isPlayer;
}

# ----------------------------------------------------------------------------
# Handler for settings page
# ----------------------------------------------------------------------------
sub handler {
	my ($class, $client, $params) = @_; 

	$log->debug("client: " . $client->name . " id: " . $client->id);
	
	my $prefEnabled = $prefs->client($client)->get('pref_Enabled');
	my $shdAddress = $prefs->client($client)->get('shdAddress');
	my $maxVol = $prefs->client($client)->get('maxVol');

	$log->debug("saveSettings: " . $params->{'saveSettings'});
	$log->debug("current values: enabled: " . $prefEnabled . " shdAddress: " . $shdAddress . " maxVol: " . $maxVol);
	$log->debug("  param values: enabled: " . $params->{'pref_Enabled'} .
                                " shdAddress: " . $params->{'shdAddress'} .
                                    " maxVol: " . $params->{'maxVol'});

	# Default maxVol to 100 if not already set.
	if (!looks_like_number($maxVol)) {
		$maxVol = 100;
		$prefs->client($client)->set('maxVol', $maxVol);
		$log->debug("default maxVol: " . $prefs->client($client)->get('maxVol'));
	}

	if ($params->{'saveSettings'}) {
		$prefs->client($client)->set('pref_Enabled', $params->{'pref_Enabled'}); 

		$shdAddress = $params->{'shdAddress'};
		# strip leading/trailing spaces
		$shdAddress =~ s/^\s+(.*)\s+/\1/;
		$prefs->client($client)->set('shdAddress', "$shdAddress"); 

		$maxVol = $params->{'maxVol'};
		if ($maxVol < 0) {
			$maxVol = 0;
		} elsif ($maxVol > 100) {
			$maxVol = 100;
		} elsif (!looks_like_number($maxVol)) {
			$maxVol = 100;
		} else {
			# strip leading/trailing spaces
			$maxVol =~ s/^\s+(.*)\s+/\1/;
		}
		$prefs->client($client)->set('maxVol', "$maxVol");
	}

	# We need to update the values on the webpage, especially
	# in case we've modified them above.
	$params->{'prefs'}->{'pref_Enabled'} = $prefs->client($client)->get('pref_Enabled'); 
	$params->{'prefs'}->{'shdAddress'} = $prefs->client($client)->get('shdAddress'); 
	$params->{'prefs'}->{'maxVol'} = $prefs->client($client)->get('maxVol'); 
	
	return $class->SUPER::handler($client, $params);
}

1;

__END__
