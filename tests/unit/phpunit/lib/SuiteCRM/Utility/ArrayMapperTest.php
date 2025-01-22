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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SuiteCRM\Utility\ArrayMapper;

class ArrayMapperTest extends SuitePHPUnitFrameworkTestCase
{
    public function test(): void
    {
        $array = [
            'object' => (object)[
                'hey' => 'there',
            ],
            'array' => [
                'foo',
                'bar',
            ],
            'address_city' => 'city',
            'address_street' => 'street',
            'email1' => 'hello@email1.com',
            'email2' => 'hello@email2.com',
            'other_field' => 'something',
            'phone' => [
                'phone_fax' => '123',
                'phone_home' => '789',
            ],
            'test' => 'deep',
            'empty' => null,
        ];

        $mappings = [
            'address_city' => 'address.city',
            'address_street' => 'address.street',
            'email1' => '+email',
            'email2' => '+email',
            'phone.phone_fax' => 'phone.fax',
            'phone.phone_home' => 'phone.home',
            'test' => 'very.very',
        ];

        $expected = [
            'object' => [
                'hey' => 'there',
            ],
            'address' => [
                'city' => 'city',
                'street' => 'street',
            ],
            'email' => [
                'hello@email1.com',
                'hello@email2.com',
            ],
            'phone' => [
                'fax' => '123',
                'home' => '789',
            ],
            'very' => [
                'very' => 'deep',
            ],
            'other_field' => 'something',
            'array' => [
                'foo',
                'bar',
            ],
        ];

        $actual = ArrayMapper::make()
            ->setMappable($array)
            ->setMappings($mappings)
            ->map();

        self::assertEquals($expected, $actual);
    }
}
