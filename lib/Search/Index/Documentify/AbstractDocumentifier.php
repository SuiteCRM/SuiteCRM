<?php
/**
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

namespace SuiteCRM\Search\Index\Documentify;

use SugarBean;
use SugarEmailAddress;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Base class all Documentifier should implement.
 *
 * A Documentifier takes a SugarBean as a parameters and converts it into an associative array
 *  suitable for document-oriented databases and json serialisation.
 */
#[\AllowDynamicProperties]
abstract class AbstractDocumentifier
{

    /**
     * Converts a bean to a document-friendly associative array.
     *
     * @param \SugarBean $bean
     *
     * @return array
     */
    abstract public function documentify(\SugarBean $bean);

    /**
     * Applies sanitizePhone() to all the phones in the serialisation array.
     *
     * @param $document
     */
    public function fixPhone(array &$document)
    {
        if (isset($document['phone'])) {
            foreach ($document['phone'] as &$phone) {
                $phone = self::sanitizePhone($phone);
            }
        }
    }

    /**
     * Attempts to fill the email field if it is empty.
     *
     * @param SugarBean $bean
     * @param array     $document
     */
    public function fixEmails(SugarBean $bean, array &$document)
    {
        if (!isset($document['email']) && $bean->hasEmails()) {
            /** @var SugarEmailAddress $emailManager */
            if (isset($bean->emailAddress)) {
                $emailManager = $bean->emailAddress;
                $email = $emailManager->getPrimaryAddress($bean);

                if (!empty($email)) {
                    $document['email'][] = $email;
                }
            }
        }
    }

    /**
     * Strips non-numeric characters from a phone number (apart from `+`), to improve search results.
     *
     * @param string $phone
     *
     * @return null|string
     */
    public function sanitizePhone($phone)
    {
        return $phone = preg_replace('/[^0-9+]/', '', $phone);
    }

    /**
     * Returns the default metadata, that are always present in a bean.
     *
     * @return string[]
     */
    protected function getMetaData()
    {
        return [
            'date_entered',
            'created_by',
            'date_modified',
            'modified_user_id',
            'assigned_user_id',
            'modified_by_name',
            'created_by_name',
            'assigned_user_name',
            'assigned_user_name_owner',
        ];
    }
}
