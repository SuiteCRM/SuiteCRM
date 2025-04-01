<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

namespace SuiteCRM\Robo\Plugin\Commands;

use Api\Core\Config\ApiConfig;
use DateTime;
use DBManager;
use OAuth2Clients;
use Robo\Tasks;
use SuiteCRM\Robo\Traits\RoboTrait;
use SuiteCRM\Robo\Traits\CliRunnerTrait;
use Api\V8\BeanDecorator\BeanManager;
use DBManagerFactory;
use User;

#[\AllowDynamicProperties]
class ApiCommands extends Tasks
{
    use RoboTrait;
    use CliRunnerTrait;

    /**
     * @var DBManager
     */
    protected $db;

    /**
     * @var BeanManager
     */
    protected $beanManager;

    /**
     * @var array
     */
    protected static $beanAliases = [
        User::class => 'Users',
        OAuth2Clients::class => 'OAuth2Clients',
    ];

    /**
     * ApiCommands constructor
     */
    public function __construct()
    {
        $this->bootstrap();
        $this->db = DBManagerFactory::getInstance();
        $this->beanManager = new BeanManager($this->db, static::$beanAliases);
    }

    /**
     * Configures the SuiteCRM V8 API with all defaults
     * @param string $name
     * @param string $password
     * @throws \Exception
     */
    public function apiConfigureV8($name, $password)
    {
        $this->say('Configure V8 Api');

        $this->taskComposerInstall()->noDev()->noInteraction()->run();
        $this->apiGenerateKeys();
        $this->apiSetKeyPermissions();
        $this->apiRebuildHtaccessFile();
        $this->apiExportPostmanENV();
        $this->apiCreateClient($name);
        $this->apiCreateUser($name, $password);
    }

    /**
     * Generate OAuth2 public/private keys
     * @param array $opts
     * @option string $privateKey set a custom path to the oauth2 private key.
     * @option string $publicKey set a custom path to the oauth2 public key.
     */
    public function apiGenerateKeys(
        $opts = ['privateKey' => ApiConfig::OAUTH2_PRIVATE_KEY, 'publicKey' => ApiConfig::OAUTH2_PUBLIC_KEY]
    ) {
        $privateKey = openssl_pkey_new(
            [
                'private_key_bits' => 2048,
                'private_key_type' => OPENSSL_KEYTYPE_RSA,
            ]
        );

        openssl_pkey_export($privateKey, $privateKeyExport);

        $publicKey = openssl_pkey_get_details($privateKey);

        $publicKeyExport = $publicKey['key'];

        file_put_contents(
            $opts['privateKey'],
            $privateKeyExport
        );

        file_put_contents(
            $opts['publicKey'],
            $publicKeyExport
        );
    }

    /**
     * Sets the Oauth2 key permissions
     * @param array $opts
     * @option string $privateKey set a custom path to the oauth2 private key.
     * @option string $publicKey set a custom path to the oauth2 public key.
     */
    public function apiSetKeyPermissions(
        $opts = ['privateKey' => ApiConfig::OAUTH2_PRIVATE_KEY, 'publicKey' => ApiConfig::OAUTH2_PUBLIC_KEY]
    ) {
        chmod(
            $opts['privateKey'],
            0600
        ) &&
        chmod(
            $opts['publicKey'],
            0600
        );
    }

    /**
     * Rebuild .Htaccess file
     */
    public function apiRebuildHtaccessFile()
    {
        @require __DIR__ . '/../../../../modules/Administration/UpgradeAccess.php';
    }


    /**
     * Creates OAuth2 client
     * @param string $name
     * @return void
     * @throws \Exception
     */
    public function apiCreateClient($name)
    {
        $count = $this->getNameCount($name, 'oauth2clients', 'name');

        $clientSecret = bin2hex(random_bytes(50));

        $clientBean = $this->beanManager->newBeanSafe(
            OAuth2Clients::class
        );

        $clientBean->name = 'V8 API Client ' . $count;
        $clientBean->secret = hash('sha256', $clientSecret);
        $clientBean->{'is_confidential'} = true;
        $clientBean->save();
        $clientBean->retrieve($clientBean->id);

        $this->outputClientCredentials(!empty($clientBean->fetched_row['id']) ? compact('clientBean',
            'clientSecret') : []);
    }

    /**
     * Creates a SuiteCRM user for the V8 API
     * @param string $name
     * @param string $password
     * @return void
     */
    public function apiCreateUser($name, $password)
    {
        $count = $this->getNameCount($name, 'users', 'user_name');

        $userBean = $this->beanManager->newBeanSafe(
            User::class
        );

        $userBean->user_name = $name . ' ' . $count;
        $userBean->first_name = 'V8';
        $userBean->last_name = 'API User';
        $userBean->email1 = 'API@example.com';
        $userBean->save();
        $userBean->setNewPassword($password, 1);
        $userBean->retrieve($userBean->id);

        $this->outputUserCredentials(!empty($userBean->fetched_row['id'])
            ? compact('userBean', 'password')
            : []);
    }

    /**
     * Export a postman environment for the V8 API
     * @param array $opts
     * @option string $postmanENV set a custom path to output a postman environment.
     */
    public function apiExportPostmanENV(
        $opts = ['postmanENV' => __DIR__ . '/../../../../Api/docs/postman/V8_API_Postman_Environment.json']
    ) {
        $rows = [
            'name' => 'SuiteCRM V8 API Environment',
            'values' => [
                [
                    'key' => 'suitecrm.url',
                    'value' => '{instance}/Api',
                    'description' => 'Used for API Operations.',
                    'enabled' => true
                ],
                [
                    'key' => 'token.url',
                    'value' => '{instance}/Api/access_token',
                    'description' => 'Used to get Access Tokens.',
                    'enabled' => true
                ]
            ]
        ];
        $json = json_encode($rows, JSON_UNESCAPED_SLASHES);

        file_put_contents($opts['postmanENV'], $json, LOCK_EX);

        $this->say('POSTMAN ENV Exported to ' . $opts['postmanENV']);
    }

    /**
     * Returns client credentials
     * @param array $client
     */
    private function outputClientCredentials(array $client)
    {
        $clientBean = $client['clientBean'];

        $clientArray = [
            'grantType' => 'Password Credentials',
            'accessToken' => '{{suitecrm.url}}/Api/access_token',
            'clientID' => $clientBean->id,
            'clientSecret' => $client['clientSecret']
        ];

        $this->io()->title('V8 API Client Credentials');

        $headers = [
            'Grant Type',
            'Access Token URL',
            'Client ID',
            'Client Secret',
        ];

        $rows = [
            $clientArray,
        ];

        $this->io->table($headers, $rows);
    }

    /**
     * Returns user credentials
     * @param array $user
     */
    private function outputUserCredentials(array $user)
    {
        $userBean = $user['userBean'];

        $userArray = [
            'name' => $userBean->user_name,
            'password' => $user['password']
        ];

        $this->io()->title('V8 API User Credentials');

        $headers = [
            'Username',
            'Password',
        ];

        $rows = [
            $userArray,
        ];

        $this->io->table($headers, $rows);
    }

    /**
     * Returns the number of duplicate name records from a table
     * @param string $name
     * @param string $table
     * @param string $row
     * @return int
     */
    private function getNameCount($name, $table, $row)
    {
        $nameQuoted = $this->db->quoted($name);

        $query = <<<SQL
SELECT
    count(`id`) AS `count`
FROM
    `$table`
WHERE
    `$row` LIKE '$nameQuoted %'
SQL;

        $result = $this->db->fetchOne($query);

        $count = $result
            ? (int)$result['count']
            : 0;

        $count++;

        return $count;
    }
}
