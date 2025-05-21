<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Repair extends Administration
{
    public function clickQuickRepairAndRebuild()
    {
        $I = $this;
        $I->visitPage('Administration', 'index');
        $I->click('#repair');
        $I->waitForText('Repair');
        $I->click('Quick Repair and Rebuild');
        $I->waitForText('Return to Administration page');
    }
}
