<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class jjwg_MapsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testjjwg_Maps(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        self::assertInstanceOf('jjwg_Maps', $jjwgMaps);
        self::assertInstanceOf('Basic', $jjwgMaps);
        self::assertInstanceOf('SugarBean', $jjwgMaps);

        self::assertEquals('jjwg_Maps', $jjwgMaps->module_dir);
        self::assertEquals('jjwg_Maps', $jjwgMaps->object_name);
        self::assertEquals('jjwg_maps', $jjwgMaps->table_name);

        self::assertEquals(true, $jjwgMaps->new_schema);
        self::assertEquals(true, $jjwgMaps->importable);
        self::assertEquals(true, $jjwgMaps->disable_row_level_security);
    }

    public function testconfiguration(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $jjwgMaps->configuration();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testsaveConfiguration(): void
    {
        self::markTestIncomplete('environment dependency');

        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with empty array/default
        $result = $jjwgMaps->saveConfiguration();
        self::assertEquals(false, $result);

        //test with data array
        $result = $jjwgMaps->saveConfiguration(array('test' => 1));
        self::assertEquals(true, $result);
    }

    public function testupdateGeocodeInfo(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');
        $bean = BeanFactory::newBean('Meetings');

        //test without bean attributes set
        $result = $jjwgMaps->updateGeocodeInfo($bean);
        self::assertFalse($result);

        //test with required attributes set
        $bean->id = 1;
        $bean->jjwg_maps_lat_c = '100';
        $bean->jjwg_maps_lng_c = '40';

        $result = $jjwgMaps->updateGeocodeInfo($bean);

        self::assertEquals(null, $result);
        self::assertEquals(100, $bean->jjwg_maps_lat_c);
        self::assertEquals(40, $bean->jjwg_maps_lng_c);


        if (!isset($bean->jjwg_maps_geocode_status_c)) {
            $beanJjwgMapsGeocodeStatusC = null;
        } else {
            $beanJjwgMapsGeocodeStatusC = $bean->jjwg_maps_geocode_status_c;
        }

        self::assertEquals('', $beanJjwgMapsGeocodeStatusC);


        if (!isset($bean->jjwg_maps_address_c)) {
            $beanJjwgMapsAddressC = null;
        } else {
            $beanJjwgMapsAddressC = $bean->jjwg_maps_address_c;
        }

        self::assertEquals('', $beanJjwgMapsAddressC);
    }

    public function testupdateRelatedMeetingsGeocodeInfo(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');
        $bean = BeanFactory::newBean('Accounts');

        //test without setting bean attributes
        $result = $jjwgMaps->updateRelatedMeetingsGeocodeInfo($bean);
        self::assertEquals(false, $result);

        //test with required attributes set
        $bean->id = 1;
        $bean->jjwg_maps_lat_c = '100';
        $bean->jjwg_maps_lng_c = '40';

        $result = $jjwgMaps->updateRelatedMeetingsGeocodeInfo($bean);
        self::assertNull($result);
        self::assertInstanceOf('jjwg_Address_Cache', $jjwgMaps->jjwg_Address_Cache);
    }

    public function testupdateMeetingGeocodeInfo(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with required attributes set
        $bean = BeanFactory::newBean('Meetings');
        $bean->id = 1;
        $bean->jjwg_maps_lat_c = '100';
        $bean->jjwg_maps_lng_c = '40';

        $result = $jjwgMaps->updateMeetingGeocodeInfo($bean);
        self::assertNull($result);
    }

    public function testupdateGeocodeInfoByAssocQuery(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with empty parameters
        $result = $jjwgMaps->updateGeocodeInfoByAssocQuery('', array(), array());
        self::assertFalse($result);

        //test with non empty but invalid parameters
        $result = $jjwgMaps->updateGeocodeInfoByAssocQuery('test', array(), array());
        self::assertFalse($result);

        //test with non empty valid parameters
        $result = $jjwgMaps->updateGeocodeInfoByAssocQuery('accounts', array('id' => 1), array());
        self::assertNull($result);
    }

    public function testupdateGeocodeInfoByBeanQuery(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');
        $bean = BeanFactory::newBean('Accounts');

        //test without setting bean attributes
        $result = $jjwgMaps->updateGeocodeInfoByBeanQuery($bean);
        self::assertFalse($result);

        //test with required attributes set
        $bean->id = 1;
        $result = $jjwgMaps->updateGeocodeInfoByBeanQuery($bean);
        self::assertNull($result);
    }

    public function testdeleteAllGeocodeInfoByBeanQuery(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');
        $bean = BeanFactory::newBean('Calls');

        //test with invalid geocode bean
        $result = $jjwgMaps->deleteAllGeocodeInfoByBeanQuery($bean);
        self::assertFalse($result);

        //test with invalid geocode bean
        $bean = BeanFactory::newBean('Accounts');
        $result = $jjwgMaps->deleteAllGeocodeInfoByBeanQuery($bean);
        self::assertNull($result);
    }

    public function testgetGeocodeAddressesResult(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with invalid geocode bean
        $result = $jjwgMaps->getGeocodeAddressesResult('calls');
        self::assertFalse($result);

        //test with invalid geocode bean
        $result = $jjwgMaps->getGeocodeAddressesResult('accounts');
        self::assertInstanceOf('mysqli_result', $result);
    }

    public function testdefineMapsAddress(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test for Account Object type
        $address = array('id' => 1, 'billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Account', $address);
        //var_dump($result);
        self::assertEquals(array('address' => 'addr 1, addr 2, addr 3, addr 4, addr 5'), $result);

        //test for Contact Object type
        $address = array('id' => 1, 'primary_address_street' => 'addr 1', 'primary_address_city' => 'addr 2', 'primary_address_state' => 'addr 3', 'primary_address_postalcode' => 'addr 4', 'primary_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Contact', $address);
        self::assertEquals(array('address' => 'addr 1, addr 2, addr 3, addr 4, addr 5'), $result);

        //test for Leads Object type
        $address = array('id' => 1, 'primary_address_street' => 'addr 1', 'primary_address_city' => 'addr 2', 'primary_address_state' => 'addr 3', 'primary_address_postalcode' => 'addr 4', 'primary_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Lead', $address);
        self::assertEquals(array('address' => 'addr 1, addr 2, addr 3, addr 4, addr 5'), $result);

        //test for Opportunities Object type
        $address = array('id' => 1, 'billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Opportunity', $address);
        self::assertEquals(false, $result);

        //test for Case Object type
        $address = array('id' => 1, 'billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Case', $address);
        self::assertEquals(false, $result);

        //test for Project Object type
        $address = array('id' => 1, 'billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Project', $address);
        self::assertEquals(false, $result);

        //test for Project Meetings type
        $address = array('id' => 1, 'billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsAddress('Meeting', $address);
        self::assertEquals(false, $result);
    }

    public function testdefineMapsFormattedAddress(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        $result = $jjwgMaps->defineMapsFormattedAddress(array());
        self::assertEquals(false, $result);

        //test for type billing
        $address = array('billing_address_street' => 'addr 1', 'billing_address_city' => 'addr 2', 'billing_address_state' => 'addr 3', 'billing_address_postalcode' => 'addr 4', 'billing_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsFormattedAddress($address, 'billing');
        self::assertEquals('addr 1, addr 2, addr 3, addr 4, addr 5', $result);

        //test for type shipping
        $address = array('shipping_address_street' => 'addr 1', 'shipping_address_city' => 'addr 2', 'shipping_address_state' => 'addr 3', 'shipping_address_postalcode' => 'addr 4', 'shipping_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsFormattedAddress($address, 'shipping');
        self::assertEquals('addr 1, addr 2, addr 3, addr 4, addr 5', $result);

        //test for type primary
        $address = array('primary_address_street' => 'addr 1', 'primary_address_city' => 'addr 2', 'primary_address_state' => 'addr 3', 'primary_address_postalcode' => 'addr 4', 'primary_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsFormattedAddress($address, 'primary');
        self::assertEquals('addr 1, addr 2, addr 3, addr 4, addr 5', $result);

        //test for type alt
        $address = array('alt_address_street' => 'addr 1', 'alt_address_city' => 'addr 2', 'alt_address_state' => 'addr 3', 'alt_address_postalcode' => 'addr 4', 'alt_address_country' => 'addr 5');
        $result = $jjwgMaps->defineMapsFormattedAddress($address, 'alt');
        self::assertEquals('addr 1, addr 2, addr 3, addr 4, addr 5', $result);

        //test for type address
        $address = array('address_street' => 'addr 1', 'address_city' => 'addr 2', 'address_state' => 'addr 3', 'address_postalcode' => 'addr 4');
        $result = $jjwgMaps->defineMapsFormattedAddress($address, 'address');
        self::assertEquals('addr 1, addr 2, addr 3, addr 4', $result);
    }

    public function testis_valid_lng(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with invalid values
        self::assertEquals(false, $jjwgMaps->is_valid_lng(''));
        self::assertEquals(false, $jjwgMaps->is_valid_lng(181));
        self::assertEquals(false, $jjwgMaps->is_valid_lng(-181));

        //test with valid values
        self::assertEquals(true, $jjwgMaps->is_valid_lng(180));
        self::assertEquals(true, $jjwgMaps->is_valid_lng(-180));
    }

    public function testis_valid_lat(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        //test with invalid values
        self::assertEquals(false, $jjwgMaps->is_valid_lat(''));
        self::assertEquals(false, $jjwgMaps->is_valid_lat(91));
        self::assertEquals(false, $jjwgMaps->is_valid_lat(-91));

        //test with valid values
        self::assertEquals(true, $jjwgMaps->is_valid_lat(90));
        self::assertEquals(true, $jjwgMaps->is_valid_lat(-90));
    }

    public function testlogGeocodeInfo(): void
    {
        $jjwgMaps = BeanFactory::newBean('jjwg_Maps');

        $bean = BeanFactory::newBean('Meetings');
        $bean->jjwg_maps_lat_c = '100';
        $bean->jjwg_maps_lng_c = '40';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $jjwgMaps->logGeocodeInfo($bean);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetProspectLists(): void
    {
        $result = getProspectLists();
        self::assertIsArray($result);
    }
}
