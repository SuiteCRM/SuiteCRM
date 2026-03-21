<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/utils/sugar_file_utils.php';

const SUITE_FILE_HASH_ALGORITHM = 'sha256';
const SUITE_FILE_HASH_LENGTH = 64;
const SUITE_FILE_SEPARATOR = '|';
const SUITE_FILE_SERIALIZED_PATTERN = '/^[aOs]:\d+:/';
const SUITE_FILE_DEFAULT_OPTIONS = ['allowed_classes' => false];

/**
 * Serializes the given data, appends its hash to ensure integrity, and writes it to the specified file.
 *
 * @param string $filename The path to the file where the serialized data will be written.
 * @param mixed $value The data to be serialized and stored in the file.
 * @return bool|int Returns the number of bytes written to the file on success, or false on failure.
 */
function suite_file_put_serialized_contents(string $filename, mixed $value): bool|int
{
    global $log;

    if (empty($filename)) {
        $log->error("[SuiteFileUtils][SuiteFilePutSerializedContents] Invalid filename provided");
        return false;
    }

    try {
        $serialized = serialize($value);
        if ($serialized === '') {
            $log->warn("[SuiteFileUtils][SuiteFilePutSerializedContents] Failed to serialize data");
            return false;
        }

        $hash = hash(SUITE_FILE_HASH_ALGORITHM, $serialized);
        $data = $hash . SUITE_FILE_SEPARATOR . $serialized;

        if (!mb_check_encoding($data, 'UTF-8')) {
            $data = mb_convert_encoding($data, 'UTF-8', 'auto');
            if ($data === false) {
                $log->error("[SuiteFileUtils][SuiteFilePutSerializedContents] Failed to convert data encoding");
                return false;
            }
        }

        return sugar_file_put_contents($filename, $data);
    } catch (Throwable $e) {
        $log->error("[SuiteFileUtils][SuiteFilePutSerializedContents] Serialization error - " . $e->getMessage());
        return false;
    }
}

/**
 * Reads the contents of a file, verifies its integrity, and unserializes the data.
 *
 * @param string $filename The path to the file from which the serialized data will be read.
 * @param array $options An optional array of options for the unserialize function, such as allowed classes.
 *                        Default value is ['allowed_classes' => false].
 * @return mixed Returns the unserialized data on success, or false on failure.
 */
function suite_file_get_unserialized_contents(string $filename, array $options = []): mixed
{
    global $log;

    if (empty($filename)) {
        $log->error("[SuiteFileUtils][SuiteFileGetUnserializedContents] Invalid filename provided");
        return false;
    }

    $data = sugar_file_get_contents($filename);
    if ($data === false) {
        return false;
    }

    if (!mb_check_encoding($data, 'UTF-8')) {
        $data = mb_convert_encoding($data, 'UTF-8', 'auto');
        if ($data === false) {
            $log->error("[SuiteFileUtils][SuiteFileGetUnserializedContents] Failed to convert data encoding");
            return false;
        }
    }

    $options = empty($options) ? SUITE_FILE_DEFAULT_OPTIONS : $options;

    try {
        $data = trim($data);
        if (empty($data)) {
            $log->debug("[SuiteFileUtils][SuiteFileGetUnserializedContents] Empty file data");
            return false;
        }

        $separatorPos = strpos($data, SUITE_FILE_SEPARATOR);
        if ($separatorPos !== false) {
            $hash = substr($data, 0, $separatorPos);
            $serializedData = substr($data, $separatorPos + 1);

            if ($separatorPos === 32 && hash('md5', $serializedData) === $hash) {
                $data = $serializedData;
            } elseif ($separatorPos === SUITE_FILE_HASH_LENGTH && hash(SUITE_FILE_HASH_ALGORITHM, $serializedData) === $hash) {
                $data = $serializedData;
            } else {
                $log->warn("[SuiteFileUtils][SuiteFileGetUnserializedContents] Data integrity check failed");
                return false;
            }
        }

        if (!preg_match(SUITE_FILE_SERIALIZED_PATTERN, $data)) {
            $log->warn("[SuiteFileUtils][SuiteFileGetUnserializedContents] Invalid serialized data format");
            return false;
        }

        $result = unserialize($data, $options);
        if ($result === false && $data !== serialize(false)) {
            $log->warn("[SuiteFileUtils][SuiteFileGetUnserializedContents] Failed to unserialize data");
            return false;
        }

        return $result;
    } catch (Throwable $e) {
        $log->error("[SuiteFileUtils][SuiteFileGetUnserializedContents] Unserialization error - " . $e->getMessage());
        return false;
    }
}