<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

/**
 * This script can be run during the update of SinergiaCRM instances. 
 * 
 * It is in charge of running any SugraCRM PHP script that will find in SticUpdates/Scripts folder
 * 
 * If you want to run just one script, you can specify the path and file name (with the extension) in the argument list . Like this:
 * http://xxxxxxxxxx.sinergiacrm.org/SticRunScripts.php?file=SticUpdates/Scripts/FixPersonalEnvironmentModuleDisplay.php
 * http://xxxxxxxxxx.sinergiacrm.org/SticRunScripts.php?file=SticUpdates/Migrations/20200803_hotfix_34_RemovingMailMergeExampleTemplates.sql
 * 
 */



/**
 * Check if the folder exists and has files to run
 * @params : $folderPath - a String containing the path to the folder.
 * @returns: Boolean     - true if empty, false otherwise
*/
function isEmptyFolder($folderPath)
{
    if (is_dir($folderPath)) {
        $archivos = scandir($folderPath);
        $archivos = array_diff($archivos, array('.', '..'));  // Removes the elements from the array: "." and ".."
        if (count($archivos) > 0) {
            return false;
        }
    }
    echo "$folderPath folder has no files to run. <br />";
    return true; 
}

/**
 * Connect to the database through PDO to be able to execute files that contain multiple SQL statements
 * @returns: PDO Object representing the connection to the database
*/
function connectToDBWithPDO()
{
    echo "Connecting to the database <br />";
    global $sugar_config;
    $mysqlHost = $sugar_config["dbconfig"]["db_host_name"];
    $mysqlDatabase = $sugar_config["dbconfig"]["db_name"];
    $mysqlUser = $sugar_config["dbconfig"]["db_user_name"];
    $mysqlPassword = $sugar_config["dbconfig"]["db_password"];
    try {
        $connection = new PDO("mysql:host=$mysqlHost;dbname=$mysqlDatabase", $mysqlUser, $mysqlPassword);
        $connection->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        $connection->exec("SET NAMES 'utf8'"); // Setting codification
        return $connection;
    } catch (PDOException $e) {
        die("Connection Error: " . $e->getMessage());
    }
}

/**
 * Executes a file with SQL statements through a PDO connection
 * @params : $connection - PDO Object representing the connection to the database
 * @params : $file       - File with SQL statements
 * @returns: boolean     - True if the execution was successful, false otherwise.
*/
function executeSQLFile($connection, $file)
{
    $sqlFileContent = file_get_contents($file);
    echo "<br> -- $file <br>";
    
    // Execute SQL statements
    try {
        $connection->exec($sqlFileContent);
        echo "Sql statements executed correctly. <br />";
        return true;
    } catch (PDOException $e) {
        echo "Error when executing sql statements: " . $e->getMessage() . "<br />";
        return false;
    }
}



// ******* SCRIPT *******
// This script runs either the script indicated in the request 
// or all scripts and migrations in the SticUpdates/Scripts/ and SticUpdates/Migrations/ folders.

include ('include/MVC/preDispatch.php');
$startTime = microtime(true);
require_once('include/entryPoint.php');
ob_start();

// Checks if a file has been indicated in the request, if it exists in the file system and executes it
if (isset($_REQUEST['file'])) {
    $file = $_REQUEST['file'];
    if (file_exists($file)) {
        $ext = substr($file, -3);
        if ($ext === "php") {
            echo "$file <br>";
            require($file);   
        } else if ($ext === "sql") {
            $connection = connectToDBWithPDO();
            executeSQLFile($connection,$file);
            // Dereferencing the PDO instance, allowing the PHP garbage collector to free the resources associated with the connection
            $connection = null;
        } else {
            echo "File: ".$file.". The file extension must be php or sql";    
        }
    } else {
        echo "File ".$file." doesn't exist in server";
    }

} else {

    echo "File isn't specified in URL";

    // SCRIPTS - Check if there are files in the scripts folder and run them
    if (!isEmptyFolder("SticUpdates/Scripts/")) 
    {
        echo "Executing all files from SticUpdates/Scripts/ <br>";
        $files = glob('SticUpdates/Scripts/*.php');
        foreach ($files as $file) {
            echo "$file <br>";
            require($file);   
        }
    }
    
    // MIGRATIONS - Check if there are files in the migrations folder and run them
    if (!isEmptyFolder("SticUpdates/Migrations/")) 
    {
        echo "Executing all SQL files from SticUpdates/Migrations/, both general and language files <br>";
        $connection = connectToDBWithPDO();

        // Run all general migrations files from the SticUpdates/Migrations/ folder
        $files = glob('SticUpdates/Migrations/*.sql');
        foreach ($files as $file) 
        {
            executeSQLFile($connection,$file);
        }
        
        // Run all languages migrations files from the SticUpdates/Migrations/<language> folder
        $language = substr($sugar_config["default_language"], 0, 2);
        $files = glob("SticUpdates/Migrations/$language/*.sql");
        foreach ($files as $file) 
        {
            executeSQLFile($connection,$file);
        }
        
        // Dereferencing the PDO instance, allowing the PHP garbage collector to free the resources associated with the connection
        $connection = null;
    }
} 