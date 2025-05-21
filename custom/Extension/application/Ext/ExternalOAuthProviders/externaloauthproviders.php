<?php

require_once 'custom/modules/ExternalOAuthConnection/provider/SticMicrosoftOAuthProviderConnector.php';
require_once 'custom/modules/ExternalOAuthConnection/provider/SticGoogleOAuthProviderConnector.php';
require_once 'custom/modules/ExternalOAuthConnection/provider/GoogleOAuthProviderConnector.php';

$aux_external_oauth_providers = [
    'Stic Google' => [
        'class' => 'SticGoogleOAuthProviderConnector'
    ],
    'Stic Microsoft' => [
        'class' => 'SticMicrosoftOAuthProviderConnector'
    ],
    'Google' => [
        'class' => 'GoogleOAuthProviderConnector'
    ],
];

$external_oauth_providers = array_merge($aux_external_oauth_providers, $external_oauth_providers);