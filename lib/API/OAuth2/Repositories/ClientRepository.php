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

namespace SuiteCRM\API\OAuth2\Repositories;

use League\OAuth2\Server\Repositories\ClientRepositoryInterface;
use SuiteCRM\API\OAuth2\Entities\ClientEntity;

#[\AllowDynamicProperties]
class ClientRepository implements ClientRepositoryInterface
{
    /**
     * {@inheritdoc}
     * @return null|ClientEntity
     */
    public function getClientEntity($clientIdentifier)
    {
        $client = new \OAuth2Clients();
        $client->retrieve($clientIdentifier);
        if (empty($client->id)) {
            return null;
        }

        $clientEntity = new ClientEntity();
        $clientEntity->setIdentifier($clientIdentifier);
        $clientEntity->setName($client->name);
        $clientEntity->setRedirectUri($client->redirect_uri ?? '');
        $clientEntity->setIsConfidential($client->is_confidential ?? false);

        return $clientEntity;
    }

    public function validateClient($clientIdentifier, $clientSecret, $grantType)
    {
        $client = new \OAuth2Clients();
        $client->retrieve($clientIdentifier);
        if (empty($client->id)) {
            return null;
        }

        if ($grantType === $client->allowed_grant_type || $grantType === 'refresh_token')
        {
            return hash('sha256', $clientSecret) === $client->secret;
        }

        return false;
    }
}
