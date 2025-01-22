<?php
/**
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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class jjwg_MarkersTest extends SuitePHPUnitFrameworkTestCase
{
    /** @noinspection PhpPossiblePolymorphicInvocationInspection */
    public function testjjwg_Markers(): void
    {
        $jjwgMarkers = BeanFactory::newBean('jjwg_Markers');

        self::assertInstanceOf('jjwg_Markers', $jjwgMarkers);
        self::assertInstanceOf('Basic', $jjwgMarkers);
        self::assertInstanceOf('SugarBean', $jjwgMarkers);

        self::assertEquals('jjwg_Markers', $jjwgMarkers->module_dir);
        self::assertEquals('jjwg_Markers', $jjwgMarkers->object_name);
        self::assertEquals('jjwg_markers', $jjwgMarkers->table_name);

        self::assertEquals(true, $jjwgMarkers->new_schema);
        self::assertEquals(true, $jjwgMarkers->importable);
        self::assertEquals(true, $jjwgMarkers->disable_row_level_security);
    }

    public function testconfiguration(): void
    {
        $jjwgMarkers = BeanFactory::newBean('jjwg_Markers');

        $jjwgMarkers->configuration();

        self::assertInstanceOf('jjwg_Maps', $jjwgMarkers->jjwg_Maps);
        self::assertIsArray($jjwgMarkers->settings);
        self::assertGreaterThan(0, count($jjwgMarkers->settings));
    }

    public function testdefine_loc(): void
    {
        $jjwgMarkers = BeanFactory::newBean('jjwg_Markers');

        //test without pre settting attributes
        $result = $jjwgMarkers->define_loc(array());
        self::assertEquals('N/A', $result['name']);
        self::assertIsNumeric($result['lat']);
        self::assertIsNumeric($result['lng']);
        self::assertEquals('company', $result['image']);

        //test with required attributes preset
        $marker = array('name' => 'test', 'lat' => 50, 'lng' => 100, 'image' => null);
        $result = $jjwgMarkers->define_loc($marker);
        self::assertSame($marker, $result);
    }

    public function testis_valid_lng(): void
    {
        $jjwgMarkers = BeanFactory::newBean('jjwg_Markers');

        //test with invalid values
        self::assertEquals(false, $jjwgMarkers->is_valid_lng(''));
        self::assertEquals(false, $jjwgMarkers->is_valid_lng(181));
        self::assertEquals(false, $jjwgMarkers->is_valid_lng(-181));

        //test with valid values
        self::assertEquals(true, $jjwgMarkers->is_valid_lng(180));
        self::assertEquals(true, $jjwgMarkers->is_valid_lng(-180));
    }

    public function testis_valid_lat(): void
    {
        $jjwgMarkers = BeanFactory::newBean('jjwg_Markers');

        //test with invalid values
        self::assertEquals(false, $jjwgMarkers->is_valid_lat(''));
        self::assertEquals(false, $jjwgMarkers->is_valid_lat(91));
        self::assertEquals(false, $jjwgMarkers->is_valid_lat(-91));

        //test with valid values
        self::assertEquals(true, $jjwgMarkers->is_valid_lat(90));
        self::assertEquals(true, $jjwgMarkers->is_valid_lat(-90));
    }
}
