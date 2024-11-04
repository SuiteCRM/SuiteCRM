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


class Basic extends SugarBean
{
    /**
     * @var array
     */
    protected static $doNotDisplayOptInTickForModule = array(
        'Users',
        'Employees'
    );

    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
    }

    /**
     * @see SugarBean::get_summary_text()
     */
    public function get_summary_text()
    {
        return (string)$this->name;
    }

    /**
     * Return Email address from an email address field eg email1
     * @param string $emailField
     * @return \EmailAddress|null
     * @throws InvalidArgumentException
     */
    public function getEmailAddressFromEmailField($emailField)
    {
        $this->validateSugarEmailAddressField($emailField);

        /** @var EmailAddress $emailAddressBean */
        $emailAddressBean = BeanFactory::getBean('EmailAddresses');

        // Fixed #5657: Only update state if email address is exist
        $emailAddressId = $this->getEmailAddressId($emailField);
        $emailAddressBean->retrieve($emailAddressId);

        return $emailAddressBean;
    }

    /**
     *
     * @param string $emailField
     * @return string|null EmailAddress ID or null on error
     * @throws \InvalidArgumentException
     */
    private function getEmailAddressId($emailField)
    {
        $log = LoggerManager::getLogger();

        $this->validateSugarEmailAddressField($emailField);
        $emailAddress = $this->cleanUpEmailAddress($this->{$emailField});

        if (!$emailAddress) {
            $log->warn('Trying to get an empty email address.');
            return null;
        }

        // List view requires us to retrieve the mail so we can see the email addresses
        if (!$this->retrieve()) {
            $log->fatal('A Basic can not retrive.');
            return null;
        }

        $found = false;
        $addresses = $this->emailAddress->addresses;
        foreach ($addresses as $address) {
            if ($this->cleanUpEmailAddress($address['email_address']) === $emailAddress) {
                $found = true;
                $emailAddressId = $address['email_address_id'];
                break;
            }
        }

        if (!$found) {
            // Changed exception to error as demo data is never selected.
            $log->fatal('A Basic bean has not selected email address. (' . $emailAddress . ')');
            return null;
        }

        return $emailAddressId;
    }

    /**
     *
     * @param string $emailField
     * @throws InvalidArgumentException
     */
    protected function validateSugarEmailAddressField($emailField)
    {
        if (!is_string($emailField)) {
            throw new InvalidArgumentException('Invalid type. $emailField must be a string value, eg. email1');
        }

        if (!preg_match('/^email\d+/', $emailField)) {
            throw new InvalidArgumentException(
                '$emailField is invalid, "' . $emailField . '" given. Expected valid name eg. email1'
            );
        }
    }

    /**
     *
     * @param string $emailAddress
     * @return string
     */
    private function cleanUpEmailAddress($emailAddress)
    {
        if (empty($emailAddress)){
            return $emailAddress;
        }

        $ret = $emailAddress;
        $ret = trim($ret);
        $ret = strtolower($ret);

        return $ret;
    }
}
