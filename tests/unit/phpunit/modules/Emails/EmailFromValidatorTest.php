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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/../../../../../modules/Emails/EmailFromValidator.php';

/**
 * EmailFromValidatorTest
 *
 * @author gyula
 */
class EmailFromValidatorTest extends SuitePHPUnitFrameworkTestCase
{
    // TODO: !@# Needs more test...
    public function testValidateWrongFromAddr(): void
    {
        $email = BeanFactory::newBean('Emails');
        $validator = new EmailFromValidator();

        // from_addr is not set
        $email->From = 'gusta@yammee.org';
        unset($email->from_addr);
        $email->FromName = 'Mr. Wee Gusta';
        $email->from_name = 'Mr. Wee Gusta';
        $email->from_addr_name = 'Mr. Wee Gusta <gusta@yammee.org>';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_ADDR_IS_NOT_SET,
        ], $errors);


        // from_addr is empty
        $email->from_addr = '';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_ADDR_IS_EMPTY,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);


        // from_addr is invalid
        $email->from_addr = 'gustayammee';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_ADDR_IS_INVALID,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);

        // from_addr is not match to From and/or from_addr_name - address part
        $email->from_addr = 'langusta@yammee.org';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);
    }

    public function testValidateWrongFrom(): void
    {
        $email = BeanFactory::newBean('Emails');
        $validator = new EmailFromValidator();

        // From is not set
        unset($email->From);
        $email->from_addr = 'gusta@yammee.org';
        $email->FromName = 'Mr. Wee Gusta';
        $email->from_name = 'Mr. Wee Gusta';
        $email->from_addr_name = 'Mr. Wee Gusta <gusta@yammee.org>';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_IS_NOT_SET,
        ], $errors);


        // From is empty
        $email->From = '';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_IS_EMPTY,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);


        // From is invalid
        $email->From = 'gustayammee';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_IS_INVALID,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);

        // From is not match to from_addr and/or from_addr_name - address part
        $email->From = 'langusta@yammee.org';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertFalse($valid);
        self::assertEquals([
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM,
            EmailFromValidator::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID,
        ], $errors);
    }

    public function testValidateOk(): void
    {
        $email = BeanFactory::newBean('Emails');
        $validator = new EmailFromValidator();

        $email->From = 'gusta@yammee.org';
        $email->from_addr = 'gusta@yammee.org';
        $email->FromName = 'Mr. Wee Gusta';
        $email->from_name = 'Mr. Wee Gusta';
        $email->from_addr_name = 'Mr. Wee Gusta <gusta@yammee.org>';
        $valid = $validator->isValid($email);
        $errors = $validator->getErrors();
        self::assertTrue($valid);
        self::assertEmpty($errors);
    }
}
