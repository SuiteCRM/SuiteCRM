<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class IssueModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Test_IssueModule&action=index';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = 'IssueTestModule';

    /**
     * @var string $NAME
     */
    public static $NAME = 'IssueTestModule';

    /**
     * @var Tester;
     */
    protected $tester;

    /**
     * IssueModule constructor.
     * @param Tester $I
     */
    public function __construct(Tester $I)
    {
        $this->tester = $I;
    }
}
