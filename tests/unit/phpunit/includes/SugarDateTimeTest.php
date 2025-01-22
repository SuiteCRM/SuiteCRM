<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Tests\Unit\includes;

use SugarDateTime;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class SugarDateTimeTest
 * @package SuiteCRM\Tests\Unit
 */
class SugarDateTimeTest extends SuitePHPUnitFrameworkTestCase
{
    public function testget_day_begin(): void
    {
        // Test that the get_day_begin function creates a SugarDateTime
        // object where the time is 00:00:00.
        $actual = (new SugarDateTime('2019-01-01'))->get_day_begin();
        $expected = new SugarDateTime('2019-01-01 00:00:00');
        self::assertEquals($expected, $actual);
    }

    public function testget_day_end(): void
    {
        // Test that the get_day_begin function creates a SugarDateTime
        // object where the time is 23:59:59.
        $actual = (new SugarDateTime('2019-01-01'))->get_day_end();
        $expected = new SugarDateTime('2019-01-01 23:59:59');
        self::assertEquals($expected, $actual);
    }

    public function testasDb(): void
    {
        // Test that the asDb function returns a string formatted for use in
        // the database.
        $actual = (new SugarDateTime('2019-1-1'))->asDb();
        $expected = '2019-01-01 00:00:00';
        self::assertEquals($expected, $actual);
    }

    public function testasDbDate(): void
    {
        // Test that the asDbDate function returns a string formatted for use
        // in the database.
        $actual = (new SugarDateTime('2019-1-1'))->asDbDate();
        $expected = '2019-01-01';
        self::assertEquals($expected, $actual);
    }

    public function testget_date_str(): void
    {
        // Test that the get_date_str function returns a string for use in
        // URL parameters.
        $actual = (new SugarDateTime('2019-1-1'))->get_date_str();
        $expected = '&year=2019&month=1&day=1&hour=0';
        self::assertEquals($expected, $actual);
    }

    public function testget_day_by_index_this_month(): void
    {
        // Test that the get_day_by_index_this_month function returns a
        // SugarDateTime representing the correct day of the month.
        $actual = (new SugarDateTime('2019-1-1'))->get_day_by_index_this_month(28);
        $expected = new SugarDateTime('2019-01-29');
        self::assertEquals($expected, $actual);
    }

    public function testget_day_by_index_this_monthWithExcessiveValue(): void
    {
        // Test that the get_day_by_index_this_month function returns a
        // SugarDateTime representing the correct day of the month, even when
        // the index provided is greater than the number of days in the month.
        $actual = (new SugarDateTime('2019-1-1'))->get_day_by_index_this_month(32);
        $expected = new SugarDateTime('2019-02-02');
        self::assertEquals($expected, $actual);
    }
}
