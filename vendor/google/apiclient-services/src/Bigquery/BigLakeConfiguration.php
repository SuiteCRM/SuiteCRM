<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Bigquery;

class BigLakeConfiguration extends \Google\Model
{
  /**
   * @var string
   */
  public $connectionId;
  /**
   * @var string
   */
  public $fileFormat;
  /**
   * @var string
   */
  public $storageUri;
  /**
   * @var string
   */
  public $tableFormat;

  /**
   * @param string
   */
  public function setConnectionId($connectionId)
  {
    $this->connectionId = $connectionId;
  }
  /**
   * @return string
   */
  public function getConnectionId()
  {
    return $this->connectionId;
  }
  /**
   * @param string
   */
  public function setFileFormat($fileFormat)
  {
    $this->fileFormat = $fileFormat;
  }
  /**
   * @return string
   */
  public function getFileFormat()
  {
    return $this->fileFormat;
  }
  /**
   * @param string
   */
  public function setStorageUri($storageUri)
  {
    $this->storageUri = $storageUri;
  }
  /**
   * @return string
   */
  public function getStorageUri()
  {
    return $this->storageUri;
  }
  /**
   * @param string
   */
  public function setTableFormat($tableFormat)
  {
    $this->tableFormat = $tableFormat;
  }
  /**
   * @return string
   */
  public function getTableFormat()
  {
    return $this->tableFormat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BigLakeConfiguration::class, 'Google_Service_Bigquery_BigLakeConfiguration');
