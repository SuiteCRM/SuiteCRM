<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\API\OAuth2;

use SuiteCRM\API\v8\Exception\ApiException;

#[\AllowDynamicProperties]
class Keys
{

    /**
     * @return bool|string
     * @throws ApiException
     */
    public function getPublicKey()
    {
        $path = __DIR__.'/public.key';
        if (!file_exists($path)) {
            $this->setUpKeys();
        }
        return file_get_contents($path);
    }

    /**
     * @return bool|string
     * @throws ApiException
     */
    public function getPrivateKey()
    {
        $path = __DIR__.'/private.key';
        if (!file_exists($path)) {
            $this->setUpKeys();
        }
        return file_get_contents($path);
    }

    /**
     * @throws ApiException
     */
    private function setUpKeys()
    {
        $config = array(
                'digest_alg' => 'sha512',
                'private_key_bits' => '2048',
                'private_key_type' => OPENSSL_KEYTYPE_RSA,
            );

        // Create the private and public key
        $resource = openssl_pkey_new($config);

        if ($resource === false) {
            throw new ApiException('[OAuth] Unable to generate private key');
        }

        // Extract the private key from $res to $privKey
        openssl_pkey_export($resource, $privateKey);
        openssl_pkey_export_to_file($privateKey, __DIR__.'/private.key');

        // Extract the public key from $res to $pubKey
        $publicKey = openssl_pkey_get_details($resource);
        if (!isset($publicKey['key'])) {
            throw new ApiException('[OAuth] Unable to generate public key');
        }
        file_put_contents(__DIR__.'/public.key', $publicKey['key']);
    }
}
