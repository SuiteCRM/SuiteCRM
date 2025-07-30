<?php

require_once 'custom/modules/ExternalOAuthConnection/provider/SticMicrosoftOAuthProviderConnector.php';
require_once 'custom/modules/ExternalOAuthConnection/provider/SticGoogleOAuthProviderConnector.php';

$aux_external_oauth_providers = [
    'Stic Google' => [
        'class' => 'SticGoogleOAuthProviderConnector'
    ],
    'Stic Microsoft' => [
        'class' => 'SticMicrosoftOAuthProviderConnector'
    ],
];

$external_oauth_providers = array_merge($aux_external_oauth_providers, $external_oauth_providers);