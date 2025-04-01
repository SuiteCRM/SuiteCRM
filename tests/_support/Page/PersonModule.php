<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class PersonModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_PersonModule&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'PersonTestModule';

    /**
     * @var string $NAME
     */
    public static $NAME = 'PersonTestModule';

    /**
     * @var Tester;
     */
    protected $tester;

    /**
     * PersonModule constructor.
     * @param Tester $I
     */
    public function __construct(Tester $I)
    {
        $this->tester = $I;
    }
}
