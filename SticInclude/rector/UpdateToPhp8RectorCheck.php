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
 * This script executes Rector during the update of SinergiaCRM instances,
 * using the Rector configuration defined in SticRectorConfig.php on the root folder
 * 
 * Without arguments, saves suggested changes to file
 * With argument "adapt-code", modifies the code 
 * 
 */

$cacheDirectory = __DIR__ . '/../../cache/rector_cached_files';
$output = new RectorCheckResultHelper(__DIR__."/../../rectorOutput.html", $cacheDirectory);

$rectorFile = __DIR__."/../vendor/rector-standalone/vendor/rector/rector/bin/rector.php";
$rectorConfigFile = __DIR__."/SticRectorConfig.php";

$output->addInfo($rectorFile, "Rector");
$output->addInfo($rectorConfigFile, "Rector config");

if (!file_exists($rectorFile)) {
    $output->addFatalError("Rector is not installed. Can not run script");
    $output->closeAndExit(1);
}

if (!file_exists($rectorConfigFile)) {
    $output->addFatalError("Missing Rector config file. Can not run script");
    $output->closeAndExit(1);
}

// Get command arguments
$arguments = $_SERVER['argv'];
$scriptName = array_shift($arguments);
$adaptCode = count($arguments) > 0 && $arguments[0] == "adapt-code";

$rectorCommand = "php {$rectorFile} process --no-progress-bar --config={$rectorConfigFile}";// --output-format json";
if (!$adaptCode) {
    $rectorCommand .= " --dry-run";
}
$output->addInfo($rectorCommand, "Command");


$descriptorspec = [
    1 => ['pipe', 'w'], // stdout
    2 => ['pipe', 'w'], // stderr
];

// Open process
$process = proc_open($rectorCommand, $descriptorspec, $pipes);
if (is_resource($process)) {
    // Read standard output
    $stdout = stream_get_contents($pipes[1]);
    $stdout = str_replace("\\n", "\n", $stdout);
    $output->setResults(htmlspecialchars($stdout));
    fclose($pipes[1]);

    // Read error output
    $stderr = stream_get_contents($pipes[2]);
    $stderr = str_replace("\\n", "\n", $stderr);
    $output->addErrorResults(htmlspecialchars($stderr));
    fclose($pipes[2]);

    // Close process
    $returnCode = proc_close($process);
    $output->closeAndExit($returnCode);
} else {
    $output->addFatalError("Can not start Rector process");
    $output->closeAndExit(1);
}

final class RectorCheckResultHelper {
    private $startDate;
    private $fileName;
    private $cacheDirectory;
    private $fatalErrors = [];
    private $infos = [];
    private $results = [];
    private $errorResults = [];
    private $isAllOk = false;
    private $numFilesWithChanges = -1;

    public function __construct($fileName, $cacheDirectory)
    {
        $this->startDate = date('Y-m-d H:i:s');

        $this->cacheDirectory = $cacheDirectory;
        $this->cleanCache();

        $this->fileName = $fileName;
        if (file_exists($this->fileName)) {
            unlink($this->fileName);
        }
    }

    private function cleanCache() {
        $this->deleteDirectoryTree($this->cacheDirectory);
    }

    private function deleteDirectoryTree($dir) {
        if (!is_dir($dir)) {
            return false; 
        }
    
        $items = scandir($dir);
        foreach ($items as $item) {
            if ($item === '.' || $item === '..') {
                continue;
            }
    
            $path = $dir . '/' . $item;
            if (is_dir($path)) {
                // Remove sub-Directories
                $this->deleteDirectoryTree($path);
            } else {
                // Remove file
                unlink($path);
            }
        }
    
        // Remove empty directory
        return rmdir($dir);
    }

    private function getLabelText($text, $label = "", $newLine = false)
    {
        if(!empty($label)) {
            $text = "<strong>{$label}</strong>: {$text}";
        }
        return $text. ($newLine ? "\n" : "");
    }
    private function getTitle($text, $newLine = false) {
        return "<h3>{$text}</h3>" . ($newLine ? "\n" : "");
    }

    public function addInfo($text, $label = "")
    {
        $this->infos[] = $this->getLabelText($text, $label);
    }

    public function addFatalError($text)
    {
        $this->fatalErrors[] = $text;
    }

    public function setResults($text)
    {
        $textArray = explode("\n", $text);
        $fileChanges = [];
        $endingFile = false;
        $fileWithChanges = false;
        $isInDiff = false;
        if (count($textArray) > 0) {
            $this->numFilesWithChanges = 0;
        }
        for ($i = 0; $i < count($textArray); $i++) {

            if ($textArray[$i] == "=====================") {
                $fileChanges = [];
                $endingFile = false;
            }
            if (empty($textArray[$i]) || 
                strpos($textArray[$i], "files with changes") !== false ||
                $textArray[$i] == "=====================") {
                continue;
            }
            if (strpos($textArray[$i], " [ERROR] ") === 0) {
                $this->addErrorResults($textArray[$i]);
                continue;
            }
            if ($textArray[$i] == "    ---------- begin diff ----------") {
                $fileChanges[count($fileChanges)-1] = "<hr /> <strong>" . $fileChanges[count($fileChanges)-1] . "</strong>";
                $fileChanges[] = $textArray[$i];
                $fileWithChanges = false;
                $isInDiff = true;
            }
            else if ($textArray[$i] == "@@ @@") {
                if (end($this->results) != "    ---------- begin diff ----------") { 
                    $fileChanges[] = "</pre></details>";
                }
                $fileChanges[] = "<details open><summary>" . $textArray[$i] . "</summary><pre style='background:lightyellow; display: inline-block;'>";
            }
            else if ($textArray[$i] == "    ----------- end diff -----------") {
                $fileChanges[] = "</pre></details>" . $textArray[$i];
                $fileChanges[] = "";
                $endingFile = true;
                $isInDiff = false;
            }
            else if ($isInDiff && strpos($textArray[$i], "-") === 0) {
                $fileChanges[] = "<div style='color:red; background:lightpink; display:inline;'>" . $textArray[$i] . "</div>";
            }
            else if ($isInDiff && strpos($textArray[$i], "+") === 0) {
                $fileChanges[] = "<div style='color:green; background:lightgreen; display:inline;'>" . $textArray[$i] . "</div>";
            }
            else {
                if ($endingFile) {
                    // Only show files with applied rules (real changes)
                    if ($textArray[$i] == "Applied rules:" ||
                        strpos($textArray[$i], " * ") === 0) {
                        $fileChanges[] = "<strong>" . $textArray[$i] . "</strong>";
                        $fileWithChanges = true;
                    } else {
                        if ($fileWithChanges) {
                            $this->results = [...$this->results, ...$fileChanges];
                            $this->results[] = "";
                            $this->results[] = "";
                            $this->numFilesWithChanges++;
                        }
                        $endingFile = false;
                        $fileChanges = [];
                        $fileChanges[] = $textArray[$i];
                    }
                } else {
                    $fileChanges[] = $textArray[$i];
                }
            }
        }
        $this->isAllOk = (strpos($text, "[OK] Rector is done!") !== false);
    }

    public function addErrorResults($text)
    {
        $this->errorResults = array_merge($this->errorResults, explode("\n", $text));
    }

    private function getIsAllOk($resultCode) {
        $this->isAllOk = ($this->isAllOk && $resultCode == 0) || 
                         ($this->numFilesWithChanges == 0 && ($resultCode == 2 || $resultCode == 0) && count($this->fatalErrors) == 0);
        return $this->isAllOk;
    }

    public function closeAndExit($resultCode)
    {
        $this->cleanCache();

        if (!$this->getIsAllOk($resultCode)) {
            $content = "Result: {$resultCode} <br />\n";

            $this->addInfo($resultCode, "Result");

            // Last Execution
            $content .= $this->getTitle("Execution", true);
            $content .= $this->getLabelText($this->startDate . "<br />", "Start", true);
            $content .= $this->getLabelText(date('Y-m-d H:i:s') . "<br />", "End", true);

            // Fatal Errors
            if (count($this->fatalErrors) > 0) {
                $content .= $this->getTitle("Error", true);
                $content .= implode("<br />\n", $this->fatalErrors);
                $content .= "<br />\n";
            }
            
            // Informations
            if (count($this->infos) > 0) {
                $content .= $this->getTitle("Information", true);
                $content .= implode("<br />\n", $this->infos);
                $content .= "<br />\n";
            }

            // Results
            if (count($this->results) > 0) {
                $content .= $this->getTitle("Result", true);
                $content .= implode("<br />\n", $this->results);
                $content .= "<br />\n";
            }

            // Error Results
            if (count($this->errorResults) > 0) {
                $content .= $this->getTitle("Rector Errors", true);
                $content .= implode("<br />\n", $this->errorResults);
                $content .= "<br />\n";
            }

            file_put_contents($this->fileName, $content);
        }
        exit($resultCode);
    }
}