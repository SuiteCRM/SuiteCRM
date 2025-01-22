<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class CompanyModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_CompanyModule&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'CompanyTestModule';

    /**
     * @var string $NAME
     */
    public static $NAME = 'CompanyTestModule';

    /**
     * @var Tester;
     */
    protected $tester;

    /**
     * BasicModule constructor.
     * @param Tester $I
     */
    public function __construct(Tester $I)
    {
        $this->tester = $I;
    }
}
