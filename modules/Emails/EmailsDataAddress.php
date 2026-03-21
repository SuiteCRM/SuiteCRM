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
 * EmailsDataAddress
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class EmailsDataAddress
{

    /**
     *
     * @param string $type
     * @param string $id
     * @param string $attributesReplyTo
     * @param string $attributesFrom
     * @param string $attributesName
     * @param string $attributesOe
     * @param string $prepend
     * @param bool $isPersonalEmailAccount
     * @param bool $isGroupEmailAccount
     * @param string $outboundEmailId
     * @param string $outboundEmailName
     * @param array $emailSignaturesArray
     * @return array
     */
    public function getDataArray(
        $type,
        $id,
        $attributesReplyTo,
        $attributesFrom,
        $attributesName,
        $attributesOe,
        $prepend,
        $isPersonalEmailAccount,
        $isGroupEmailAccount,
        $outboundEmailId,
        $outboundEmailName,
        $emailSignaturesArray,
        $accountName = '',
        $attributesReplyToName = ''
    ) {
        $signatureResolver = new EmailsSignatureResolver();
        $signatureResolver->setSignatureArray($emailSignaturesArray);

        $dataArray = [
            'type' => $type,
            'id' => $id,
            'name' => $accountName,
            'attributes' => $this->getDataArrayAttributes(
                $attributesReplyTo,
                $attributesFrom,
                $attributesName,
                $attributesOe,
                $attributesReplyToName
            ),
            'prepend' => $prepend,
            'isPersonalEmailAccount' => $isPersonalEmailAccount,
            'isGroupEmailAccount' => $isGroupEmailAccount,
            'outboundEmail' => [
                'id' => $outboundEmailId,
                'name' => $outboundEmailName,
            ],
            'emailSignatures' => [
                'html' => mb_convert_encoding(html_entity_decode($signatureResolver->getHtml() ?? ''), 'UTF-8', 'ISO-8859-1'),
                'plain' => $signatureResolver->getPlaintext(),
                'no_default_available' => $signatureResolver->isNoDefaultAvailable(),
            ],
        ];

        return $dataArray;
    }

    /**
     *
     * @param string $attributesReplyTo
     * @param string $attributesFrom
     * @param string $attributesName
     * @param string $attributesOe
     * @param string $attributesReplyToName
     * @return array
     */
    protected function getDataArrayAttributes($attributesReplyTo, $attributesFrom, $attributesName, $attributesOe, $attributesReplyToName = '')
    {
        return [
            'reply_to' => mb_convert_encoding($attributesReplyTo, 'UTF-8', 'ISO-8859-1'),
            'reply_to_name' => mb_convert_encoding($attributesReplyToName, 'UTF-8', 'ISO-8859-1'),
            'from' => mb_convert_encoding($attributesFrom, 'UTF-8', 'ISO-8859-1'),
            'name' => mb_convert_encoding($attributesName, 'UTF-8', 'ISO-8859-1'),
            'oe' => mb_convert_encoding($attributesOe, 'UTF-8', 'ISO-8859-1'),
        ];
    }
}
