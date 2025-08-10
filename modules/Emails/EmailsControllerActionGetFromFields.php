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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/../../include/SugarEmailAddress/SugarEmailAddress.php';
require_once __DIR__ . '/EmailsSignatureResolver.php';
require_once __DIR__ . '/EmailsDataAddress.php';
require_once __DIR__ . '/EmailsDataAddressCollector.php';

/**
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class EmailsControllerActionGetFromFields
{

    /**
     *
     * @var User
     */
    protected $currentUser;

    /**
     *
     * @var EmailsDataAddressCollector
     */
    protected $collector;

    /**
     *
     * @param User $currentUser
     * @param EmailsDataAddressCollector $collector
     */
    public function __construct(User $currentUser, EmailsDataAddressCollector $collector)
    {
        $this->currentUser = $currentUser;
        $this->collector = $collector;
    }

    /**
     *
     * @param Email $email
     * @param InboundEmail $ie
     * @return string JSON
     */
    public function handleActionGetFromFields(Email $email, InboundEmail $ie)
    {
        $email->email2init();
        $ie->email = $email;
        $ieAccounts = $ie->retrieveAllByGroupIdWithGroupAccounts($this->currentUser->id);
        $accountSignatures = $this->currentUser->getPreference('account_signatures', 'Emails');
        $showFolders = sugar_unserialize(base64_decode($this->currentUser->getPreference('showFolders', 'Emails'))) ?: [];
        $emailSignatures = $this->getEmailSignatures($accountSignatures);
        $defaultEmailSignature = $this->getDefaultSignatures();
        $prependSignature = $this->currentUser->getPreference('signature_prepend') ?? false;
        $dataAddresses = $this->collector->collectDataAddressesFromIEAccounts(
            $ieAccounts,
            $showFolders,
            $prependSignature,
            $emailSignatures,
            $defaultEmailSignature
        );

        $dataAddresses = $dataAddresses ?? [];

        $this->addOutboundEmailAccounts($dataAddresses, $prependSignature, $defaultEmailSignature);

        $dataEncoded = json_encode(array('data' => $dataAddresses), JSON_UNESCAPED_UNICODE);
        $results = mb_convert_encoding($dataEncoded, 'ISO-8859-1');
        return $results;
    }

    /**
     * Get Outbound from fields
     * @param Email $email
     * @return string JSON
     * @throws JsonException
     */
    public function getOutboundFromFields(Email $email)
    {
        global $log;
        $email->email2init();

        $dataAddresses = [];

        $this->addOutboundEmailAccounts($dataAddresses);

        $this->collector->addSystemEmailAddress($dataAddresses);

        $dataEncoded = [];
        try {
            $dataEncoded = json_encode(array('data' => $dataAddresses), JSON_THROW_ON_ERROR | JSON_UNESCAPED_UNICODE);
        } catch (Exception $e) {
            $log->fatal('getOutboundFromFields | unable to json encode the addresses for from fields | message: ' . $e->getMessage() ?? '');
        }

        return mb_convert_encoding($dataEncoded, 'ISO-8859-1');
    }

    /**
     *
     * @param string|null $accountSignatures
     * @return array|null
     */
    protected function getEmailSignatures($accountSignatures = null)
    {
        if ($accountSignatures != null) {
            $emailSignatures = sugar_unserialize(base64_decode($accountSignatures));
        } else {
            $GLOBALS['log']->warn('User ' . $this->currentUser->name . ' does not have a signature');
            $emailSignatures = null;
        }

        return $emailSignatures;
    }

    /**
     *
     * @return array
     */
    protected function getDefaultSignatures()
    {
        $defaultEmailSignature = $this->currentUser->getDefaultSignature();
        if (empty($defaultEmailSignature)) {
            $defaultEmailSignature = array(
                'html' => '<br>',
                'plain' => '\r\n',
            );
            $defaultEmailSignature['no_default_available'] = true;
        } else {
            $defaultEmailSignature['no_default_available'] = false;
        }

        return $defaultEmailSignature;
    }

    /**
     * @param array $dataAddresses
     * @return void
     */
    protected function addOutboundEmailAccounts(array &$dataAddresses, bool $prependSignature = false, array $defaultEmailSignature = []): void
    {
        /** @var OutboundEmailAccounts $outboundAccount */
        $outboundAccount = BeanFactory::newBean('OutboundEmailAccounts');

        /** @var OutboundEmailAccounts[] $userOutboundAccounts */
        $userOutboundAccounts = $outboundAccount->getUserOutboundAccounts();

        foreach ($userOutboundAccounts as $userOutboundAccount) {

            $id = $userOutboundAccount->id ?? '';
            $name = $userOutboundAccount->name ?? '';
            $fromAddress = $userOutboundAccount->getFromAddress();
            $fromName = $userOutboundAccount->getFromName();
            $replyToAddress = $userOutboundAccount->getReplyToAddress();
            $replyToName = $userOutboundAccount->getReplyToName();
            $type = $userOutboundAccount->type ?? '';
            $signature = $defaultEmailSignature['signature_html'] ?? $userOutboundAccount->signature ?? '';
            $isPersonal = $type === 'user';
            $isGroup = $type === 'group';
            $entry = [
                'type' => 'OutboundEmailAccount',
                'id' => $id,
                'name' => $name,
                'attributes' => [
                    'from' => $fromAddress,
                    'name' => $fromName,
                    'oe' => '',
                    'reply_to' => $replyToAddress,
                    'reply_to_name' => $replyToName
                ],
                'prepend' => $prependSignature,
                'isPersonalEmailAccount' => $isPersonal,
                'isGroupEmailAccount' => $isGroup,
                'emailSignatures' => [
                    'html' => mb_convert_encoding(html_entity_decode((string) $signature), 'UTF-8', 'ISO-8859-1'),
                    'plain' => ''
                ]
            ];

            $dataAddresses[] = $entry;
        }
    }
}
