<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ReleaseTest extends SuitePHPUnitFrameworkTestCase
{
    public function testRelease(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $release = BeanFactory::newBean('Releases');

        self::assertInstanceOf('Release', $release);
        self::assertInstanceOf('SugarBean', $release);

        self::assertEquals('releases', $release->table_name);
        self::assertEquals('Releases', $release->module_dir);
        self::assertEquals('Release', $release->object_name);

        self::assertEquals(true, $release->new_schema);
    }

    public function testget_summary_text(): void
    {
        $release = BeanFactory::newBean('Releases');

        //test without setting name
        self::assertEquals(null, $release->get_summary_text());

        //test with name set
        $release->name = 'test';
        self::assertEquals('test', $release->get_summary_text());
    }

    public function testget_releases(): void
    {
        $release = BeanFactory::newBean('Releases');

        //test with default params
        $result = $release->get_releases();
        self::assertIsArray($result);

        //test with custom params
        $result = $release->get_releases(true, 'Hidden', 'name is not null');
        self::assertIsArray($result);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $release = BeanFactory::newBean('Releases');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $release->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $release = BeanFactory::newBean('Releases');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $release->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $release = BeanFactory::newBean('Releases');

        $release->name = 'test';
        $release->status = 'Hidden';

        $expected = array(
                    'NAME' => 'test',
                    'STATUS' => 'Hidden',
                    'ENCODED_NAME' => 'test',
                    'ENCODED_STATUS' => null,
        );

        $actual = $release->get_list_view_data();

        self::assertSame($expected, $actual);
    }

    public function testbuild_generic_where_clause(): void
    {
        $release = BeanFactory::newBean('Releases');

        //test with empty string params
        $expected = "name like '%'";
        $actual = $release->build_generic_where_clause('');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "name like 'test%'";
        $actual = $release->build_generic_where_clause('test');
        self::assertSame($expected, $actual);
    }
}
