<?php

namespace Page;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class AccountsModule
{
    /**
     * @var string $URL include url of current page
     */
    public static $URL = 'index.php?module=Accounts';

    /**
     * @var string $NAME
     */
    public static $PACKAGE_NAME = '';

    /**
     * @var string $NAME
     */
    public static $NAME = 'Accounts';

    /**
     * @var string
     */
    public static $CREATE_LINK = 'Create Account';

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
