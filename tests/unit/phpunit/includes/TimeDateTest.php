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

use BeanFactory;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use TimeDate;
use UserPreference;

class TimeDateTest extends SuitePHPUnitFrameworkTestCase
{
    public function testget_date_format(): void
    {
        // Validate that it gets the date format from the user's preferences.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('datef', 'Y-m-d');

        $actual = (new TimeDate($user))->get_date_format();
        $expected = 'Y-m-d';
        self::assertEquals($expected, $actual);
    }

    public function testget_time_format(): void
    {
        // Validate that it gets the time format from the user's preferences.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('timef', 'H:i:s');

        $actual = (new TimeDate($user))->get_time_format();
        $expected = 'H:i:s';
        self::assertEquals($expected, $actual);
    }

    public function testget_date_time_format(): void
    {
        // Validate that it gets the date time format from the user's preferences.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('datef', 'Y-m-d');
        $userPreference->setPreference('timef', 'H:i:s');

        $actual = (new TimeDate($user))->get_date_time_format();
        $expected = 'Y-m-d H:i:s';
        self::assertEquals($expected, $actual);
    }

    public function testget_first_day_of_week(): void
    {
        // Validate that it gets the first day of the week from the user's
        // preferences.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('fdow', 1);

        $actual = (new TimeDate($user))->get_first_day_of_week();
        $expected = 1;
        self::assertEquals($expected, $actual);
    }

    public function testget_first_day_of_week_defaultResponse(): void
    {
        // When no user is specified for the TimeDate,
        // it defaults to 0 aka Sunday.
        $actual = (new TimeDate())->get_first_day_of_week();
        $expected = 0;
        self::assertEquals($expected, $actual);
    }

    public function testmerge_date_time(): void
    {
        // Merges the date and time formats given two strings.
        // Literally just puts a space in between them.
        $actual = (new TimeDate())->merge_date_time('Y-m-d', 'H:i:s');
        $expected = 'Y-m-d H:i:s';
        self::assertEquals($expected, $actual);
    }

    public function testsplit_date_time(): void
    {
        // Splits the date time format into an array of two items when given
        // a valid string.
        $actual = (new TimeDate())->split_date_time('Y-m-d H:i:s');
        $expected = ['Y-m-d', 'H:i:s'];
        self::assertEquals($expected, $actual);
    }

    public function testhttpTime(): void
    {
        // Returns a timestamp in the format defined by RFC 2616.
        $actual = (new TimeDate())::httpTime(1546300800);
        $expected = 'Tue, 01 Jan 2019 00:00:00 GMT';
        self::assertEquals($expected, $actual);
    }

    public function testto_db_time(): void
    {
        // Test that the function returns the time but not the date, even if
        // a date is provided.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('datef', 'Y-m-d');
        $userPreference->setPreference('timef', 'H:i:s');

        $timeDate = new TimeDate($user);

        $actual = $timeDate->to_db_time('2019-01-01 11:00:00');
        $expected = '11:00:00';
        self::assertEquals($expected, $actual);

        $actual2 = $timeDate->to_db_time('23:30:00');
        $expected2 = '23:30:00';
        self::assertEquals($expected2, $actual2);
    }

    public function testto_db_date_time(): void
    {
        // Test that the function returns the full date time as an array.
        // We create a user here, but it doesn't actually take the user's
        // preferences into account. This should probably be fixed at some
        // point.
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');
        $userPreference = new UserPreference($user);
        $userPreference->setPreference('datef', 'Y-m-d');
        $userPreference->setPreference('timef', 'H:i:s');
        $actual = (new TimeDate($user))->to_db_date_time('2019-01-01', '11:00:00');
        $expected = ['2019-01-01', '11:00:00'];
        self::assertEquals($expected, $actual);
    }

    public function testsplitTime(): void
    {
        // Split time when the input is only a time represented as a string.
        $actual = (new TimeDate())->splitTime('11:30:00', 'H:i:s');
        $expected = [
            'h' => '11',
            'm' => '30',
            's' => '00'
        ];
        self::assertEquals($expected, $actual);
    }

    public function testsplitTimeWith24HourDateTime(): void
    {
        // Split time when it has a full date time and uses 24-hour time.
        $actual = (new TimeDate())->splitTime('2019-01-01 23:30:15', 'Y-m-d H:i:s');
        $expected = [
            'h' => '23',
            'm' => '30',
            's' => '15'
        ];
        self::assertEquals($expected, $actual);
    }

    public function testsplitTimeWithPM(): void
    {
        // Split time when it has a full date time and AM/PM.
        $actual = (new TimeDate())->splitTime('2019-01-01 9:15:01 PM', 'Y-m-d H:i:s A');
        $expected = [
            'h' => '09',
            'm' => '15',
            's' => '01',
            'a' => 'PM'
        ];
        self::assertEquals($expected, $actual);
    }
}
