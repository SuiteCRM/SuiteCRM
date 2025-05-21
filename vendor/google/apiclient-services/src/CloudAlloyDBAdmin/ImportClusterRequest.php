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

namespace Google\Service\CloudAlloyDBAdmin;

class ImportClusterRequest extends \Google\Model
{
  protected $csvImportOptionsType = CsvImportOptions::class;
  protected $csvImportOptionsDataType = '';
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $gcsUri;
  protected $sqlImportOptionsType = SqlImportOptions::class;
  protected $sqlImportOptionsDataType = '';
  /**
   * @var string
   */
  public $user;

  /**
   * @param CsvImportOptions
   */
  public function setCsvImportOptions(CsvImportOptions $csvImportOptions)
  {
    $this->csvImportOptions = $csvImportOptions;
  }
  /**
   * @return CsvImportOptions
   */
  public function getCsvImportOptions()
  {
    return $this->csvImportOptions;
  }
  /**
   * @param string
   */
  public function setDatabase($database)
  {
    $this->database = $database;
  }
  /**
   * @return string
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string
   */
  public function setGcsUri($gcsUri)
  {
    $this->gcsUri = $gcsUri;
  }
  /**
   * @return string
   */
  public function getGcsUri()
  {
    return $this->gcsUri;
  }
  /**
   * @param SqlImportOptions
   */
  public function setSqlImportOptions(SqlImportOptions $sqlImportOptions)
  {
    $this->sqlImportOptions = $sqlImportOptions;
  }
  /**
   * @return SqlImportOptions
   */
  public function getSqlImportOptions()
  {
    return $this->sqlImportOptions;
  }
  /**
   * @param string
   */
  public function setUser($user)
  {
    $this->user = $user;
  }
  /**
   * @return string
   */
  public function getUser()
  {
    return $this->user;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImportClusterRequest::class, 'Google_Service_CloudAlloyDBAdmin_ImportClusterRequest');
