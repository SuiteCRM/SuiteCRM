<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class SaleModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_SaleModule&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'SaleTestModule';

    /**
     * @var string $NAME
     */
    public static $NAME = 'SaleTestModule';

    /**
     * @var Tester;
     */
    protected $tester;

    /**
     * SaleModule constructor.
     * @param Tester $I
     */
    public function __construct(Tester $I)
    {
        $this->tester = $I;
    }
}
