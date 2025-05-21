<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class FileModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_FileModule&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'FileTestModule';

    /**
     * @var string $NAME
     */
    public static $NAME = 'FileTestModule';

    /**
     * @var Tester;
     */
    protected $tester;

    /**
     * FileModule constructor.
     * @param Tester $I
     */
    public function __construct(Tester $I)
    {
        $this->tester = $I;
    }
}
