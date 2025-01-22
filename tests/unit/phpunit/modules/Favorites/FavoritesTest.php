<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class FavoritesTest extends SuitePHPUnitFrameworkTestCase
{
    public function testFavorites(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $favorites = BeanFactory::newBean('Favorites');
        self::assertInstanceOf('Favorites', $favorites);
        self::assertInstanceOf('Basic', $favorites);
        self::assertInstanceOf('SugarBean', $favorites);

        self::assertEquals('Favorites', $favorites->module_dir);
        self::assertEquals('Favorites', $favorites->object_name);
        self::assertEquals('favorites', $favorites->table_name);
        self::assertEquals(true, $favorites->new_schema);
    }

    public function testdeleteFavorite(): void
    {
        //testing with an empty ID
        $result = BeanFactory::newBean('Favorites')->deleteFavorite('');
        self::assertEquals(false, $result);
    }

    public function testgetFavoriteID(): void
    {
        $favorites = BeanFactory::newBean('Favorites');

        //test with blank string parameters
        $result = $favorites->getFavoriteID('', '');
        self::assertEquals(false, $result);

        //test with string parameters
        $result = $favorites->getFavoriteID('Accounts', '1');
        self::assertEquals(false, $result);
    }

    public function testgetCurrentUserSidebarFavorites(): void
    {
        $favorites = BeanFactory::newBean('Favorites');

        //test with empty string parameter
        $result = $favorites->getCurrentUserSidebarFavorites();
        self::assertIsArray($result);

        //test with string
        $result = $favorites->getCurrentUserSidebarFavorites('1');
        self::assertIsArray($result);
    }
}
