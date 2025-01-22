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

/**
 * EmailsDataAddressCollector
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class EmailsDataAddressCollector
{
    public const ERR_INVALID_INBOUND_EMAIL_TYPE = 201;
    public const ERR_STORED_OUTBOUND_EMAIL_NOT_SET = 202;
    public const ERR_STORED_OUTBOUND_EMAIL_ID_IS_INVALID = 203;
    public const ERR_REPLY_TO_ADDR_NOT_FOUND = 204;
    public const ERR_REPLY_TO_FORMAT_INVALID_SPLITS = 205;
    public const ERR_STORED_OUTBOUND_EMAIL_NOT_FOUND = 206;
    public const ERR_REPLY_TO_FORMAT_INVALID_NO_NAME = 207;
    public const ERR_REPLY_TO_FORMAT_INVALID_NO_ADDR = 208;
    public const ERR_REPLY_TO_FORMAT_INVALID_AS_FROM = 209;

    /**
     *
     * @var User
     */
    protected $currentUser;

    /**
     *
     * @var array
     */
    protected $sugarConfig;

    /**
     *
     * @var int
     */
    protected $err;

    /**
     *
     * @var OutboundEmail
     */
    protected $oe;

    // ------------------ FROM DATA STRUCT -------------------

    /**
     *
     * @var string
     */
    protected $replyTo;

    /**
     *
     * @var string
     */
    protected $fromAddr;

    /**
     *
     * @var string
     */
    protected $fromName;

    /**
     *
     * @var string
     */
    protected $oeId;

    /**
     *
     * @var string
     */
    protected $oeName;

    // -------------------------------------------------------

    /**
     *
     * @param User $currentUser
     * @param array $sugarConfig
     */
    public function __construct(User $currentUser, $sugarConfig)
    {
        $this->currentUser = $currentUser;
        $this->sugarConfig = $sugarConfig;
    }

    /**
     * @param $ieAccounts
     * @param $showFolders
     * @param $prependSignature
     * @param $emailSignatures
     * @param $defaultEmailSignature
     * @return array
     * @throws EmailValidatorException
     */
    public function collectDataAddressesFromIEAccounts(
        $ieAccounts,
        $showFolders,
        $prependSignature,
        $emailSignatures,
        $defaultEmailSignature
    ) {
        $dataAddresses = array();
        foreach ($ieAccounts as $inboundEmail) {
            $this->validateInboundEmail($inboundEmail);

            if (in_array($inboundEmail->id, $showFolders)) {
                $storedOptions = sugar_unserialize(base64_decode($inboundEmail->stored_options));
                $isGroupEmailAccount = $inboundEmail->isGroupEmailAccount();
                $isPersonalEmailAccount = $inboundEmail->isPersonalEmailAccount();

                // if group email account, check that user is allowed to use group email account
                $inboundEmailStoredOptions = $inboundEmail->getStoredOptions();
                if ($isGroupEmailAccount && !isTrue($inboundEmailStoredOptions['allow_outbound_group_usage'] ?? false)) {
                    continue;
                }

                $this->getOutboundEmailOrError($storedOptions, $inboundEmail);
                $this->retrieveFromDataStruct($storedOptions);

                $emailFromValidator = new EmailFromValidator();

                $this->logReplyToError($emailFromValidator);


                $dataAddress = $this->getDataAddressFromIEAccounts(
                    $inboundEmail,
                    $storedOptions,
                    $prependSignature,
                    $isPersonalEmailAccount,
                    $isGroupEmailAccount,
                    $emailSignatures,
                    $defaultEmailSignature
                );

                $dataAddresses[] = $dataAddress;
            }
        }

        return $this->fillDataAddress($dataAddresses, $defaultEmailSignature, $prependSignature);
    }


    /**
     *
     * @param InboundEmail $inboundEmail
     * @throws InvalidArgumentException
     */
    protected function validateInboundEmail($inboundEmail = null)
    {
        if (!$inboundEmail instanceof InboundEmail) {
            throw new InvalidArgumentException(
                'Inbound Email Account should be a valid Inbound Email. ' . gettype($inboundEmail) . ' given.',
                self::ERR_INVALID_INBOUND_EMAIL_TYPE
            );
        }
    }

    /**
     *
     * @param array $storedOptions
     * @param InboundEmail $inboundEmail
     */
    protected function getOutboundEmailOrError($storedOptions, InboundEmail $inboundEmail)
    {
        $this->err = null;
        $this->setOe(null);
        if (!isset($storedOptions['outbound_email'])) {
            // exception
            LoggerManager::getLogger()->error('EmailController::action_getFromFields() expects an outbound email id as stored option of inbound email (' . $inboundEmail->id . ') but it isn\'t set.');
            $this->err = self::ERR_STORED_OUTBOUND_EMAIL_NOT_SET;
        } else {
            $validator = new SuiteCRM\Utility\SuiteValidator();
            if ($validator->isValidId($storedOptions['outbound_email'])) {
                // exception
                LoggerManager::getLogger()->error('EmailController::action_getFromFields() expects an outbound email id as stored option of inbound email (' . $inboundEmail->id . ') but it isn\'t valid.');
                $this->err = self::ERR_STORED_OUTBOUND_EMAIL_ID_IS_INVALID;
            } else {
                $this->setOe($this->getOutboundEmailOrErrorByStoredOptions($storedOptions));
            }
        }
    }

    /**
     *
     * @param OutboundEmail|null $oe
     */
    protected function setOe($oe)
    {
        $this->oe = $oe;
    }

    /**
     *
     * @param array $storedOptions
     */
    protected function retrieveFromDataStruct($storedOptions)
    {
        if ($this->err) {
            LoggerManager::getLogger()->error('EmailController::action_getFromFields() panic: An error occurred! (' . $this->err . ')');

            $this->replyTo = $this->getReplyToOnError($storedOptions);
            $this->fromName = $this->getFromNameOnError($storedOptions);
            $this->fromAddr = $this->getFromAddrOnError($storedOptions);
            $this->oeId = null;
            $this->oeName = null;
        } else {
            $this->replyTo = mb_convert_encoding($storedOptions['reply_to_addr'], 'UTF-8', 'ISO-8859-1');
            $this->fromName = mb_convert_encoding($storedOptions['from_name'], 'UTF-8', 'ISO-8859-1');
            $this->fromAddr = mb_convert_encoding($storedOptions['from_addr'], 'UTF-8', 'ISO-8859-1');
            $this->oeId = $this->oe->id;
            $this->oeName = $this->oe->name;
        }
    }

    /**
     * @param EmailFromValidator $emailFromValidator
     * @throws EmailValidatorException
     */
    protected function logReplyToError(EmailFromValidator $emailFromValidator)
    {
        if (!$this->replyTo) {
            // exception
            LoggerManager::getLogger()->error('EmailController::action_getFromFields() panic: An Outbound Email Reply-to Address is not found.');
            $replyToErr = self::ERR_REPLY_TO_ADDR_NOT_FOUND;
        } else {
            $splits = explode(' ', $this->replyTo);
            if (count($splits) !== 2) {
                LoggerManager::getLogger()->error('Incorrect "replay to" format found: ' . $this->replyTo);
                $replyToErr = self::ERR_REPLY_TO_FORMAT_INVALID_SPLITS;
            } else {
                $replyToErr = null;
                $tmpName = $this->getTmpNameForLogReplyToError($splits, $replyToErr);
                $tmpAddr = $this->getTmpAddrForLogReplyToError($splits, $replyToErr);

                $this->validateForLogReplyToError($tmpName, $tmpAddr, $emailFromValidator, $replyToErr);
            }
        }

        if (isset($replyToErr) && $replyToErr) {
            // exception
            LoggerManager::getLogger()->error('EmailController::action_getFromFields() error: ' . $replyToErr);
        }
    }

    /**
     *
     * @param array $splits
     * @param int $replyToErr
     * @return string
     */
    protected function getTmpNameForLogReplyToError($splits, &$replyToErr)
    {
        if (!isset($splits[0])) {
            LoggerManager::getLogger()->error('Reply-to name part not found: ' . $this->getReplyTo());
            $replyToErr = self::ERR_REPLY_TO_FORMAT_INVALID_NO_NAME;
        }

        return isset($splits[0]) ? $splits[0] : null;
    }

    /**
     *
     * @param array $splits
     * @param int $replyToErr
     * @return string
     */
    protected function getTmpAddrForLogReplyToError($splits, &$replyToErr)
    {
        if (!isset($splits[1])) {
            LoggerManager::getLogger()->error('Reply-to email address part not found: ' . $this->getReplyTo());
            $replyToErr = self::ERR_REPLY_TO_FORMAT_INVALID_NO_ADDR;
        }

        return isset($splits[1]) ? $splits[1] : null;
    }

    /**
     * @param $tmpName
     * @param $tmpAddr
     * @param EmailFromValidator $emailFromValidator
     * @param $replyToErr
     * @throws EmailValidatorException
     */
    protected function validateForLogReplyToError(
        $tmpName,
        $tmpAddr,
        EmailFromValidator $emailFromValidator,
        &$replyToErr
    ) {
        $tmpEmail = BeanFactory::newBean('Emails');
        $tmpEmail->FromName = $tmpEmail->from_name = $tmpName;
        $tmpEmail->From = $tmpEmail->from_addr = $tmpAddr;
        $tmpEmail->from_addr_name = $this->getReplyTo();

        if (!$emailFromValidator->isValid($tmpEmail)) {
            // exception
            LoggerManager::getLogger()->error('EmailController::action_getFromFields() panic: An Outbound Email Reply-to Address is invalid.');
            $replyToErr = self::ERR_REPLY_TO_FORMAT_INVALID_AS_FROM;
        }
    }

    /**
     *
     * @return string
     */
    protected function getReplyTo()
    {
        return $this->replyTo;
    }

    /**
     *
     * @param InboundEmail $inboundEmail
     * @param array $storedOptions
     * @param string $prependSignature
     * @param bool $isPersonalEmailAccount
     * @param bool $isGroupEmailAccount
     * @param array $emailSignatures
     * @param array $defaultEmailSignature
     * @return array
     */
    protected function getDataAddressFromIEAccounts(
        InboundEmail $inboundEmail,
        $storedOptions,
        $prependSignature,
        $isPersonalEmailAccount,
        $isGroupEmailAccount,
        $emailSignatures,
        $defaultEmailSignature
    ) {
        $dataAddress = $this->getDataAddressArrayFromIEAccounts(
            $inboundEmail,
            $storedOptions,
            $prependSignature,
            $isPersonalEmailAccount,
            $isGroupEmailAccount
        );

        $emailSignatureId = $this->getEmailSignatureId($emailSignatures, $inboundEmail);

        $signature = $this->currentUser->getSignature($emailSignatureId);
        if (!$signature) {
            if ($defaultEmailSignature['no_default_available'] === true) {
                $dataAddress['emailSignatures'] = $defaultEmailSignature;
            } else {
                $dataAddress['emailSignatures'] = array(
                    'html' => mb_convert_encoding(html_entity_decode((string) $defaultEmailSignature['signature_html']), 'UTF-8', 'ISO-8859-1'),
                    'plain' => $defaultEmailSignature['signature'],
                );
            }
        } else {
            $dataAddress['emailSignatures'] = array(
                'html' => mb_convert_encoding(html_entity_decode((string) $signature['signature_html']), 'UTF-8', 'ISO-8859-1'),
                'plain' => $signature['signature'],
            );
        }

        return $dataAddress;
    }


    /**
     *
     * @param InboundEmail $inboundEmail
     * @param array $storedOptions
     * @param string $prependSignature
     * @param bool $isPersonalEmailAccount
     * @param bool $isGroupEmailAccount
     * @return array
     */
    protected function getDataAddressArrayFromIEAccounts(
        InboundEmail $inboundEmail,
        $storedOptions,
        $prependSignature,
        $isPersonalEmailAccount,
        $isGroupEmailAccount
    ) {
        $dataAddress = new EmailsDataAddress();

        return $dataAddress->getDataArray(
            $inboundEmail->module_name,
            $inboundEmail->id,
            $storedOptions['reply_to_addr'],
            $storedOptions['from_addr'],
            $storedOptions['from_name'],
            null,
            $prependSignature,
            $isPersonalEmailAccount,
            $isGroupEmailAccount,
            $this->getOeId(),
            $this->getOeName(),
            [],
            $inboundEmail->name,
            $storedOptions['reply_to_name'] ?? ''
        );
    }

    /**
     *
     * @return string
     */
    protected function getOeId()
    {
        return $this->oeId;
    }

    /**
     *
     * @return string
     */
    protected function getOeName()
    {
        return $this->oeName;
    }

    /**
     *
     * @param array $emailSignatures
     * @param InboundEmail $inboundEmail
     * @return string
     */
    protected function getEmailSignatureId($emailSignatures, InboundEmail $inboundEmail)
    {

        // Include signature
        if (isset($emailSignatures[$inboundEmail->id]) && !empty($emailSignatures[$inboundEmail->id])) {
            $emailSignatureId = $emailSignatures[$inboundEmail->id];
        } else {
            $emailSignatureId = '';
        }

        return $emailSignatureId;
    }

    /**
     *
     * @param array $dataAddresses
     * @param array $defaultEmailSignature
     * @param string $prependSignature
     * @return array
     */
    protected function fillDataAddress($dataAddresses, $defaultEmailSignature, $prependSignature)
    {
        $dataAddressesWithUserAddresses = $this->fillDataAddressFromUserAddresses(
            $dataAddresses,
            $defaultEmailSignature,
            $prependSignature
        );
        $dataAddressesWithUserAddressesAndSystem = $this->fillDataAddressWithSystemMailerSettings(
            $dataAddressesWithUserAddresses,
            $defaultEmailSignature
        );

        return
            $this->fillDataAddressFromPersonal(
                $dataAddressesWithUserAddressesAndSystem
            );
    }

    /**
     *
     * @param array $dataAddresses
     * @param array $defaultEmailSignature
     * @param string $prependSignature
     * @return array
     */
    protected function fillDataAddressFromUserAddresses($dataAddresses, $defaultEmailSignature, $prependSignature)
    {
        if (isset($this->sugarConfig['email_allow_send_as_user']) && $this->sugarConfig['email_allow_send_as_user']) {
            $sugarEmailAddress = new SugarEmailAddress();
            $userAddressesArr = $sugarEmailAddress->getAddressesByGUID($this->currentUser->id, 'Users');
            $dataAddresses = $this->collectDataAddressesFromUserAddresses(
                $dataAddresses,
                $userAddressesArr,
                $defaultEmailSignature,
                $prependSignature
            );
        }

        return $dataAddresses;
    }


    /**
     *
     * @param array $dataAddresses
     * @param array $userAddressesArr
     * @param array $defaultEmailSignature
     * @param string $prependSignature
     * @return array
     */
    protected function collectDataAddressesFromUserAddresses(
        $dataAddresses,
        $userAddressesArr,
        $defaultEmailSignature,
        $prependSignature
    ) {
        foreach ($userAddressesArr as $userAddress) {
            if (!isset($userAddress['reply_to_addr']) || !$userAddress['reply_to_addr']) {
                LoggerManager::getLogger()->error('EmailController::action_getFromFields() is Panicking: Reply-To address is not filled.');
            }
            $fromString = $this->getFromString($userAddress);
            $signatureHtml = $this->getSignatureHtml($defaultEmailSignature);
            $signatureTxt = $this->getSignatureTxt($defaultEmailSignature);

            $dataAddresses[] = $this->getCollectDataAddressArrayFromUserAddresses(
                $userAddress,
                $fromString,
                $prependSignature,
                $signatureHtml,
                $signatureTxt
            );
        }

        return $dataAddresses;
    }

    /**
     *
     * @param array $userAddress
     * @return string
     */
    protected function getFromString($userAddress)
    {
        if (isset($userAddress['reply_to_addr']) && $userAddress['reply_to_addr'] === '1') {
            $fromString = $this->currentUser->full_name . ' &lt;' . $userAddress['email_address'] . '&gt;';
        } else {
            $fromString = $this->currentUser->full_name . ' &lt;' . $this->currentUser->email1 . '&gt;';
        }

        return $fromString;
    }

    /**
     * @param $email
     * @return string
     */
    protected function addCurrentUserToEmailString($email)
    {
        return $this->currentUser->full_name . ' &lt;' . $email . '&gt;';
    }

    /**
     *
     * @param array $defaultEmailSignature
     * @return string
     */
    protected function getSignatureHtml($defaultEmailSignature)
    {
        if (!isset($defaultEmailSignature['signature_html'])) {
            LoggerManager::getLogger()->warn('EmailController::action_getFromFields() is Panicking: Default email signature array does not have index as signature_html');
            $signatureHtml = null;
        } else {
            $signatureHtml = $defaultEmailSignature['signature_html'];
        }

        return $signatureHtml;
    }

    /**
     *
     * @param array $defaultEmailSignature
     * @return string
     */
    protected function getSignatureTxt($defaultEmailSignature)
    {
        if (!isset($defaultEmailSignature['signature'])) {
            LoggerManager::getLogger()->warn('EmailController::action_getFromFields() is Panicking: Default email signature array does not have index as signature');
            $signatureTxt = null;
        } else {
            $signatureTxt = $defaultEmailSignature['signature'];
        }

        return $signatureTxt;
    }

    /**
     *
     * @param array $userAddress
     * @param string $fromString
     * @param string $prependSignature
     * @param string $signatureHtml
     * @param string $signatureTxt
     * @return array
     */
    protected function getCollectDataAddressArrayFromUserAddresses(
        $userAddress,
        $fromString,
        $prependSignature,
        $signatureHtml,
        $signatureTxt
    ) {
        $dataAddress = new EmailsDataAddress();

        return $dataAddress->getDataArray(
            'personal',
            $userAddress['email_address_id'],
            $this->currentUser->full_name . ' &lt;' . $userAddress['email_address'] . '&gt;',
            $fromString,
            $this->currentUser->full_name,
            null,
            $prependSignature,
            true,
            false,
            null,
            null,
            [
                'html' => mb_convert_encoding(html_entity_decode($signatureHtml), 'UTF-8', 'ISO-8859-1'),
                'plain' => $signatureTxt,
            ],
            $userAddress['email_address']
        );
    }

    /**
     *
     * @param array $dataAddresses
     * @param array $defaultEmailSignature
     * @return array
     */
    protected function fillDataAddressWithSystemMailerSettings($dataAddresses, $defaultEmailSignature)
    {
        $this->setOe(new OutboundEmail());
        if ($this->getOe()->isAllowUserAccessToSystemDefaultOutbound()) {
            $system = $this->getOe()->getSystemMailerSettings();
            $dataAddresses[] = $this->getFillDataAddressArray(
                $system->id,
                $system->name,
                $system->smtp_from_name,
                $system->smtp_from_addr,
                $system->mail_smtpuser,
                $defaultEmailSignature
            );
        }

        return $dataAddresses;
    }

    /**
     * Add system email address
     * @param array $dataAddresses
     */
    public function addSystemEmailAddress(array &$dataAddresses): void
    {
        $this->setOe(new OutboundEmail());
        if ($this->getOe()->isAllowUserAccessToSystemDefaultOutbound()) {
            $system = $this->getOe()->getSystemMailerSettings();
            $dataAddresses[] = $this->getFillDataAddressArray(
                $system->id,
                $system->name,
                $system->smtp_from_name,
                $system->smtp_from_addr,
                $system->mail_smtpuser,
                []
            );
        }
    }

    /**
     * @param $dataAddresses
     * @return mixed
     */
    protected function fillDataAddressFromPersonal($dataAddresses)
    {
        foreach ($dataAddresses as $address => $userAddress) {
            if ($userAddress['type'] !== 'system') {
                $emailInfo = $userAddress['attributes'];
                $fromString = $emailInfo['from'];
                $replyString = $emailInfo['reply_to'];

                $dataAddresses[$address]['attributes'] = [
                    'from' => $fromString,
                    'name' => $userAddress['attributes']['name'],
                    'oe' => $userAddress['attributes']['oe'],
                    'reply_to' => $replyString,
                    'reply_to_name' => $emailInfo['reply_to_name'] ?? ''
                ];
            }
        }

        return $dataAddresses;
    }

    /**
     *
     * @return OutboundEmail
     */
    protected function getOe()
    {
        return $this->oe;
    }


    /**
     *
     * @param string $id
     * @param string $name
     * @param string $fromName
     * @param string $fromAddr
     * @param string $mailUser
     * @param array $defaultEmailSignature
     * @return array
     */
    protected function getFillDataAddressArray(
        $id,
        $name,
        $fromName,
        $fromAddr,
        $mailUser,
        $defaultEmailSignature
    ) {
        $dataAddress = new EmailsDataAddress();

        return $dataAddress->getDataArray(
            'system',
            $id,
            $fromAddr,
            $fromAddr,
            $fromName,
            false,
            false,
            true,
            $id,
            $name,
            $mailUser,
            $defaultEmailSignature,
            'System',
            $fromName
        );
    }

    /**
     *
     * @param array $storedOptions
     * @return OutboundEmail
     */
    protected function getOutboundEmailOrErrorByStoredOptions($storedOptions)
    {
        $this->oe = new OutboundEmail();
        if (!$this->oe->retrieve($storedOptions['outbound_email'])) {
            // exception
            LoggerManager::getLogger()->error('Trying to retrieve an OutboundEmail by ID: ' . $storedOptions['outbound_email'] . ' but it is not found.');
            $this->err = self::ERR_STORED_OUTBOUND_EMAIL_NOT_FOUND;
        }

        return $this->oe;
    }

    /**
     *
     * @param array $storedOptions
     * @return string
     */
    protected function getReplyToOnError($storedOptions)
    {
        if (!isset($storedOptions['reply_to_addr'])) {
            LoggerManager::getLogger()->warn('Stored reply to address is not set.');
        } elseif (!$storedOptions['reply_to_addr']) {
            LoggerManager::getLogger()->warn('Stored reply to address is not filled.');
        }
        $this->replyTo = isset($storedOptions['reply_to_addr']) ? $storedOptions['reply_to_addr'] : null;

        return $this->replyTo;
    }

    /**
     *
     * @param array $storedOptions
     * @return string
     */
    protected function getFromNameOnError($storedOptions)
    {
        if (!isset($storedOptions['from_name'])) {
            LoggerManager::getLogger()->warn('Stored from name is not set.');
        } elseif (!$storedOptions['from_name']) {
            LoggerManager::getLogger()->warn('Stored from name is not filled.');
        }
        $this->fromName = isset($storedOptions['from_name']) ? $storedOptions['from_name'] : null;

        return $this->fromName;
    }

    /**
     *
     * @param array $storedOptions
     * @return string
     */
    protected function getFromAddrOnError($storedOptions)
    {
        if (!isset($storedOptions['from_addr'])) {
            LoggerManager::getLogger()->warn('Stored from address is not set.');
        } elseif (!$storedOptions['from_addr']) {
            LoggerManager::getLogger()->warn('Stored from address is not filled.');
        }
        $this->fromAddr = isset($storedOptions['from_addr']) ? $storedOptions['from_addr'] : null;

        return $this->fromAddr;
    }
}
