<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class ModuleFields
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_ModuleFields&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'TestModuleFields';

    /**
     * @var string $NAME
     */
    public static $NAME = 'TestModuleFields';

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
