<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

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

class PersonTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var \UnitTester
     */
    protected $tester;

    public function testSetLawfulBasis(): void
    {
        $person = BeanFactory::newBean('Contacts');
        $person->last_name = 'Smith';
        $person->createdAuditRecords = false;

        // Test when  basis is not a string
        try {
            $person->setLawfulBasis(1, '');
            self::assertTrue(false);
        } catch (InvalidArgumentException $ex) {
            self::assertEquals('basis must be a string', $ex->getMessage());
        }

        // test when basis does not exist
        try {
            $person->setLawfulBasis('Test Invalid Basis', '');
            self::assertTrue(false);
        } catch (InvalidArgumentException $ex) {
            self::assertEquals('invalid lawful basis', $ex->getMessage());
        }

        // test valid basis
        self::assertEquals(1, $person->setLawfulBasis('', ''));
        self::assertEquals(1, $person->setLawfulBasis('consent', ''));
        self::assertEquals(1, $person->setLawfulBasis('contract', ''));
        self::assertEquals(1, $person->setLawfulBasis('legal_obligation', ''));
        self::assertEquals(1, $person->setLawfulBasis('protection_of_interest', ''));
        self::assertEquals(1, $person->setLawfulBasis('public_interest', ''));
        self::assertEquals(1, $person->setLawfulBasis('legitimate_interest', ''));
        self::assertEquals(1, $person->setLawfulBasis('withdrawn', ''));

        // test lawful basis has been set
        $person->setLawfulBasis('consent', '');
        self::assertEquals('^consent^', $person->lawful_basis);

        // Test when source is not a string
        try {
            $person->setLawfulBasis('', 1);
            self::assertTrue(false);
        } catch (InvalidArgumentException $ex) {
            self::assertEquals('source for lawful basis must be a string', $ex->getMessage());
        }

        // test when source does not exist
        try {
            $person->setLawfulBasis('', 'Test Invalid Sources');
            self::assertTrue(false);
        } catch (InvalidArgumentException $ex) {
            self::assertEquals('invalid lawful basis source', $ex->getMessage());
        }

        // test lawful sources
        self::assertEquals(true, $person->setLawfulBasis('', ''));
        self::assertEquals(true, $person->setLawfulBasis('', 'website'));
        self::assertEquals(true, $person->setLawfulBasis('', 'phone'));
        self::assertEquals(true, $person->setLawfulBasis('', 'given_to_user'));
        self::assertEquals(true, $person->setLawfulBasis('', 'email'));
        self::assertEquals(true, $person->setLawfulBasis('', 'third_party'));

        // test that source is being set
        self::assertEquals('third_party', $person->lawful_basis_source);
    }
}
