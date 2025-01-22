<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class jjwg_Address_CacheTest extends SuitePHPUnitFrameworkTestCase
{
    public function testjjwg_Address_Cache(): void
    {
        // execute the constructor and check for the Object type and attributes
        $jjwgAddressCache = BeanFactory::newBean('jjwg_Address_Cache');
        self::assertInstanceOf('jjwg_Address_Cache', $jjwgAddressCache);
        self::assertInstanceOf('Basic', $jjwgAddressCache);
        self::assertInstanceOf('SugarBean', $jjwgAddressCache);

        self::assertEquals('jjwg_Address_Cache', $jjwgAddressCache->module_dir);
        self::assertEquals('jjwg_Address_Cache', $jjwgAddressCache->object_name);
        self::assertEquals('jjwg_address_cache', $jjwgAddressCache->table_name);

        self::assertEquals(true, $jjwgAddressCache->new_schema);
        self::assertEquals(true, $jjwgAddressCache->importable);
        self::assertEquals(true, $jjwgAddressCache->disable_row_level_security);
    }

    public function testconfiguration(): void
    {
        $jjwgAddressCache = BeanFactory::newBean('jjwg_Address_Cache');
        $jjwgAddressCache->configuration();

        self::assertInstanceOf('jjwg_Maps', $jjwgAddressCache->jjwg_Maps);
        self::assertIsArray($jjwgAddressCache->settings);
        self::assertGreaterThan(0, count($jjwgAddressCache->settings));
    }

    public function testSaveAndGetAddressCacheInfoAndDeleteAllAddressCache(): void
    {
        $jjwgAddressCache = BeanFactory::newBean('jjwg_Address_Cache');

        //test saveAddressCacheInfo() with empty info array
        $ainfo = array();
        $result = $jjwgAddressCache->saveAddressCacheInfo($ainfo);
        self::assertEquals(false, $result);

        //test saveAddressCacheInfo() with a valid info array
        $jjwgAddressCache->settings['address_cache_save_enabled'] = 1;
        $ainfo = array('address' => 'test', 'lat' => '24.861462', 'lng' => '67.009939', 'description' => 'test description');
        $result = $jjwgAddressCache->saveAddressCacheInfo($ainfo);
        self::assertEquals(true, $result);

        //test getAddressCacheInfo() with empty info array
        $result = $jjwgAddressCache->getAddressCacheInfo(array());
        self::assertEquals(false, $result);

        //test getAddressCacheInfo() with a valid info array
        $jjwgAddressCache->settings['address_cache_get_enabled'] = 1;
        $ainfo = array('address' => 'test', 'lat' => '24.861462', 'lng' => '67.009939', 'description' => 'test description');
        $result = $jjwgAddressCache->getAddressCacheInfo($ainfo);
        self::assertIsArray($result);

        //test deleteAllAddressCache
        $jjwgAddressCache->deleteAllAddressCache();

        //verify that record cannot be retrieved anynore
        $result = $jjwgAddressCache->getAddressCacheInfo($ainfo);
        self::assertEquals(false, $result);
    }

    public function testis_valid_lng(): void
    {
        $jjwgAddressCache = BeanFactory::newBean('jjwg_Address_Cache');

        //test with invalid values
        self::assertEquals(false, $jjwgAddressCache->is_valid_lng(''));
        self::assertEquals(false, $jjwgAddressCache->is_valid_lng(181));
        self::assertEquals(false, $jjwgAddressCache->is_valid_lng(-181));

        //test with valid values
        self::assertEquals(true, $jjwgAddressCache->is_valid_lng(180));
        self::assertEquals(true, $jjwgAddressCache->is_valid_lng(-180));
    }

    public function testis_valid_lat(): void
    {
        self::markTestIncomplete('Incorrect state hash (in PHPUnitTest): Hash doesn\'t match at key "database::email_addresses".');

        // test
        $jjwgAddressCache = BeanFactory::newBean('jjwg_Address_Cache');

        //test with invalid values
        self::assertEquals(false, $jjwgAddressCache->is_valid_lat(''));
        self::assertEquals(false, $jjwgAddressCache->is_valid_lat(91));
        self::assertEquals(false, $jjwgAddressCache->is_valid_lat(-91));

        //test with valid values
        self::assertEquals(true, $jjwgAddressCache->is_valid_lat(90));
        self::assertEquals(true, $jjwgAddressCache->is_valid_lat(-90));
    }
}
