<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class jjwg_AreasTest extends SuitePHPUnitFrameworkTestCase
{
    public function testjjwg_Areas(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        self::assertInstanceOf('jjwg_Areas', $jjwgAreas);
        self::assertInstanceOf('Basic', $jjwgAreas);
        self::assertInstanceOf('SugarBean', $jjwgAreas);

        self::assertEquals('jjwg_Areas', $jjwgAreas->module_dir);
        self::assertEquals('jjwg_Areas', $jjwgAreas->object_name);
        self::assertEquals('jjwg_areas', $jjwgAreas->table_name);

        self::assertEquals(true, $jjwgAreas->new_schema);
        self::assertEquals(true, $jjwgAreas->importable);
        self::assertEquals(true, $jjwgAreas->disable_row_level_security);

        self::assertEquals(null, $jjwgAreas->polygon);
        self::assertEquals(true, $jjwgAreas->point_on_vertex);
        self::assertEquals(0, $jjwgAreas->area);
        self::assertEquals(null, $jjwgAreas->centroid);
    }

    public function testconfiguration(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');
        $jjwgAreas->configuration();

        self::assertInstanceOf('jjwg_Maps', $jjwgAreas->jjwg_Maps);
        self::assertIsArray($jjwgAreas->settings);
        self::assertGreaterThan(0, count($jjwgAreas->settings));
    }

    public function testretrieve(): void
    {
        self::markTestSkipped('Skipped pending feedback on the area behaviour.');
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without pre settting attributes
        $jjwgAreas->retrieve();
        self::assertEquals(false, $jjwgAreas->polygon);
        self::assertEquals(0, $jjwgAreas->area);
        self::assertEquals(null, $jjwgAreas->centroid);

        //test with required attributes preset
        $jjwgAreas->coordinates = "100,80,10\r\n101,81,11\r\n102,82,12";

        $expected_polygon = array(
                array('lng' => '100', 'lat' => '80', 'elv' => '10'),
                array('lng' => '101', 'lat' => '81', 'elv' => '11'),
                array('lng' => '102', 'lat' => '82', 'elv' => '12'),
        );
        $expected_centroid = array('lng' => 67.3333333333333285963817615993320941925048828125, 'lat' => 54.0, 'elv' => 0);

        $jjwgAreas->retrieve();

        self::assertSame($expected_polygon, $jjwgAreas->polygon);
        self::assertEquals(20, $jjwgAreas->area);
        self::assertSame($expected_centroid, $jjwgAreas->centroid);
    }

    public function testdefine_polygon(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without pre settting attributes
        $actual = $jjwgAreas->define_polygon();
        self::assertEquals(false, $actual);

        //test with required attributes preset
        $jjwgAreas->coordinates = "100,80,10\r\n101,81,11\r\n102,82,12";
        $expected = array(
                      array('lng' => '100', 'lat' => '80', 'elv' => '10'),
                      array('lng' => '101', 'lat' => '81', 'elv' => '11'),
                      array('lng' => '102', 'lat' => '82', 'elv' => '12'),
                );
        $actual = $jjwgAreas->define_polygon();
        self::assertSame($actual, $expected);
    }

    public function testdefine_area_loc(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without pre settting attributes
        $result = $jjwgAreas->define_area_loc();
        self::assertEquals('N/A', $result['name']);
        self::assertIsNumeric($result['lat']);
        self::assertIsNumeric($result['lng']);

        //test with required attributes preset
        $jjwgAreas->name = 'test';
        $jjwgAreas->centroid = array('lng' => 100, 'lat' => 50);

        $expected = array('name' => 'test', 'lat' => 50, 'lng' => 100);
        $result = $jjwgAreas->define_area_loc();
        self::assertSame($expected, $result);
    }

    public function testdefine_centroid(): void
    {
        self::markTestSkipped('Skipped pending feedback on the area behaviour.');
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without setting up coordinates
        $result = $jjwgAreas->define_centroid();
        self::assertEquals(null, $result);

        //test with coordinates setup
        $jjwgAreas->coordinates = "100,80,10\r\n101,81,11\r\n102,82,12";
        $expected = array('lng' => 67.3333333333333285963817615993320941925048828125, 'lat' => 54.0, 'elv' => 0);

        $result = $jjwgAreas->define_centroid();
        self::assertSame($expected, $result);
    }

    public function testdefine_area(): void
    {
        self::markTestSkipped('Skipped pending feedback on the area behaviour.');
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without setting up coordinates
        $result = $jjwgAreas->define_area();
        self::assertEquals(0, $result);

        //test with coordinates setup
        $jjwgAreas->coordinates = "100,80,10\r\n101,81,11\r\n102,82,12";

        $result = $jjwgAreas->define_area();
        self::assertEquals(20, $result);
    }

    public function testdefine_loc(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without pre settting attributes
        $result = $jjwgAreas->define_loc(array());
        self::assertEquals('N/A', $result['name']);
        self::assertIsNumeric($result['lat']);
        self::assertIsNumeric($result['lng']);

        //test with required attributes preset
        $marker = array('name' => 'test', 'lat' => 50, 'lng' => 100);
        $result = $jjwgAreas->define_loc($marker);
        self::assertSame($marker, $result);
    }

    public function testis_valid_lng(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test with invalid values
        self::assertEquals(false, $jjwgAreas->is_valid_lng(''));
        self::assertEquals(false, $jjwgAreas->is_valid_lng(181));
        self::assertEquals(false, $jjwgAreas->is_valid_lng(-181));

        //test with valid values
        self::assertEquals(true, $jjwgAreas->is_valid_lng(180));
        self::assertEquals(true, $jjwgAreas->is_valid_lng(-180));
    }

    public function testis_valid_lat(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test with invalid values
        self::assertEquals(false, $jjwgAreas->is_valid_lat(''));
        self::assertEquals(false, $jjwgAreas->is_valid_lat(91));
        self::assertEquals(false, $jjwgAreas->is_valid_lat(-91));

        //test with valid values
        self::assertEquals(true, $jjwgAreas->is_valid_lat(90));
        self::assertEquals(true, $jjwgAreas->is_valid_lat(-90));
    }

    public function testis_marker_in_area(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        $marker = array('name' => 'test', 'lat' => 100, 'lng' => 40);

        //test without setting up coordinates
        self::assertEquals(false, $jjwgAreas->is_marker_in_area($marker));

        //test with coordinates set
        $jjwgAreas->coordinates = '100,40,0.0 101,81,0.0 102,32,0.0';
        self::assertEquals(false, $jjwgAreas->is_marker_in_area($marker));
    }

    public function testis_point_in_area(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without setting up coordinates
        self::assertEquals(false, $jjwgAreas->is_point_in_area(100, 40));

        //test with coordinates set
        $jjwgAreas->coordinates = '100,40,10 101,81,11 102,82,12';
        self::assertEquals(false, $jjwgAreas->is_point_in_area(101, 40));
        self::assertEquals(true, $jjwgAreas->is_point_in_area(100, 40));
    }

    public function testpoint_in_polygon(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        //test without setting up coordinates
        self::assertEquals(false, $jjwgAreas->point_in_polygon('100,40,0.0'));

        //test with coordinates set
        $jjwgAreas->coordinates = '100,40,10 101,81,11 102,82,12';
        self::assertEquals(true, $jjwgAreas->point_in_polygon('100,40,0.0'));
    }

    public function testpoint_on_vertex(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        $vertices = array('100,40,10', '101,81,11', '102,82,12');
        self::assertEquals(false, $jjwgAreas->point_on_vertex('100,40,0.0', $vertices));
        self::assertEquals(true, $jjwgAreas->point_on_vertex('100,40,10', $vertices));
    }

    public function testpoint_string_to_coordinates(): void
    {
        $jjwgAreas = BeanFactory::newBean('jjwg_Areas');

        $expected = array('x' => 100, 'y' => 40);
        $actual = $jjwgAreas->point_string_to_coordinates('100,40,10');
        self::assertEquals($expected, $actual);
    }
}
