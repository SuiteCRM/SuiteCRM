<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Accounts extends \AcceptanceTester
{
    /**
     * Create an account
     *
     * @param string $name
     * @return string Account ID
     */
    public function createAccount($name)
    {
        global $db;
        $id = create_guid();
        $accountType = 'Customer';

        $query = "INSERT INTO accounts (id, name, account_type, date_entered) VALUES ('?', '?', '?', '?')";

        $db->pQuery($query, array(
            $id,
            $name,
            $accountType,
            date('Y-m-d H:i:s')
        ));

        return $id;
    }
}
