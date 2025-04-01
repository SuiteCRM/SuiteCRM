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
 * Class EntryPointConfirmOptInHandler
 */
#[\AllowDynamicProperties]
class EntryPointConfirmOptInHandler
{

    /**
     * @var EmailAddress $emailAddress
     */
    private $emailAddress;

    /**
     *
     * @param array $request
     * @param array $post
     */
    public function __construct($request = null, $post = null)
    {
        if (is_null($request)) {
            $request = $_REQUEST;
        }

        if (is_null($post)) {
            $post = $_POST;
        }

        $method = isset($request['method']) && $request['method'] ? $request['method'] : null;

        $output = $this->callMethod($method, $post, $request);

        echo $output;
        sugar_cleanup();
    }

    /**
     *
     * @param string $method
     * @param array $post
     * @param array $request
     * @return string
     */
    protected function callMethod($method, $post, $request)
    {
        switch ($method) {
            case 'confirmOptInSelected':
                $output = $this->methodConfirmOptInSelected($post);
                break;
            default:
                $output = $this->methodConfirmOptInUser($request);
                break;
        }
        return $output;
    }

    /**
     * @global array $app_strings
     * @param array $post
     * @return string|boolean
     */
    private function methodConfirmOptInSelected($post)
    {
        global $app_strings;

        $configurator = new Configurator();
        if (!$configurator->isConfirmOptInEnabled()) {
            return false;
        }

        $module = $post['module'];
        $uids = explode(',', $post['uid']);
        $confirmedOptInEmailsSent = 0;
        $errors = 0;
        $warnings = 0;
        $msg = '';

        foreach ($uids as $uid) {
            $emailMan = BeanFactory::newBean('EmailMan');
            if (!$emailMan->addOptInEmailToEmailQueue($module, $uid)) {
                $errors++;
            } elseif ($emailMan->getLastOptInWarn()) {
                $warnings++;
            } else {
                $confirmedOptInEmailsSent++;
            }
        }

        if ($confirmedOptInEmailsSent > 0) {
            $msg .= sprintf($app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL'], $confirmedOptInEmailsSent);
        }

        if ($warnings > 0) {
            $msg .=  sprintf($app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_NOT_OPT_IN'], $warnings);
        }

        if ($errors > 0) {
            $msg .=  sprintf($app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_MISSING_EMAIL_ADDRESS_ID'], $errors);
        }


        return $msg;
    }

    /**
     * Confirm Opt In User
     *
     * @param array $request
     * @return string
     */
    private function methodConfirmOptInUser($request)
    {
        $emailAddress = BeanFactory::getBean('EmailAddresses');
        $this->emailAddress = $emailAddress->retrieve_by_string_fields([
            'confirm_opt_in_token' => $request['from']
        ]);

        if ($this->emailAddress) {
            $this->emailAddress->confirmOptIn();
            $this->emailAddress->save();

            $people = $this->getIDs($this->emailAddress->email_address, 'Contacts');
            if ($people) {
                $this->setLawfulBasisForEachPerson($people, 'Contacts');
            }
            $people = $this->getIDs($this->emailAddress->email_address, 'Leads');
            if ($people) {
                $this->setLawfulBasisForEachPerson($people, 'Leads');
            }

            $people = $this->getIDs($this->emailAddress->email_address, 'Prospects');
            if ($people) {
                $this->setLawfulBasisForEachPerson($people, 'Prospects');
            }
        }
        $template = new Sugar_Smarty();
        $template->assign('FOCUS', $this->emailAddress);

        return $template->fetch('include/EntryPointConfirmOptIn.tpl');
    }

    /**
     * @param String $email
     * @param String $module
     *
     * @return array|bool
     */
    private function getIDs($email, $module)
    {
        $people = $this->emailAddress->getRelatedId($email, $module);
        return $people;
    }

    /**
     * @param array $people
     */
    private function setLawfulBasisForEachPerson(array $people, $module)
    {
        /** @var Person $person */
        foreach ($people as $person) {
            $bean = BeanFactory::getBean($module, $person);
            if ($bean) {
                if (!$bean->setLawfulBasis('consent', 'email')) {
                    LoggerManager::getLogger()->warn('Lawful basis saving failed for record ' . $bean->name);
                }
            }
        }
    }
}
