<?php
/**
 * Backwards compatibility for OneLogin\Saml2
 */
$saml2_class_names = [
    'Auth',
    'AuthnRequest',
    'Constants',
    'Error',
    'ValidationError',
    'IdPMetadataParser',
    'LogoutRequest',
    'LogoutResponse',
    'Metadata',
    'Response',
    'Settings',
    'Utils',
];

foreach ($saml2_class_names as $name) {
    class_alias("OneLogin\\Saml2\\" . $name, 'OneLogin_Saml2_' . $name);
}

if (!class_exists(OneLogin_Saml2_Auth::class)) {
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Auth {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_AuthnRequest {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Constants {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Error {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_ValidationError {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_IdPMetadataParser {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_LogoutRequest {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_LogoutResponse {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Metadata {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Response {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Settings {}
    /** @deprecated this is an alias for OneLogin\\Saml2 */
    #[\AllowDynamicProperties]
    class OneLogin_Saml2_Utils {}
}


/**
 * Backwards compatibility for Zend
 */
class_alias('SuiteCRM\\Zend_Oauth_Provider', 'Zend\\Oauth\\Provider');

if (!class_exists(Provider::class)) {
    /** @deprecated this is an alias for Zend_Oauth_Provider */
    #[\AllowDynamicProperties]
    class Provider {}
}
