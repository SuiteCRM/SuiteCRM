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

namespace Google\Service\Datastream;

class BlmtConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $bucket;
  /**
   * @var string
   */
  public $connectionName;
  /**
   * @var string
   */
  public $fileFormat;
  /**
   * @var string
   */
  public $rootPath;
  /**
   * @var string
   */
  public $tableFormat;

  /**
   * @param string
   */
  public function setBucket($bucket)
  {
    $this->bucket = $bucket;
  }
  /**
   * @return string
   */
  public function getBucket()
  {
    return $this->bucket;
  }
  /**
   * @param string
   */
  public function setConnectionName($connectionName)
  {
    $this->connectionName = $connectionName;
  }
  /**
   * @return string
   */
  public function getConnectionName()
  {
    return $this->connectionName;
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
  public function setRootPath($rootPath)
  {
    $this->rootPath = $rootPath;
  }
  /**
   * @return string
   */
  public function getRootPath()
  {
    return $this->rootPath;
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
class_alias(BlmtConfig::class, 'Google_Service_Datastream_BlmtConfig');
