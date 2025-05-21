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

namespace Google\Service\CloudNaturalLanguage;

class XPSTableSpec extends \Google\Model
{
  protected $columnSpecsType = XPSColumnSpec::class;
  protected $columnSpecsDataType = 'map';
  /**
   * @var string
   */
  public $importedDataSizeInBytes;
  /**
   * @var string
   */
  public $rowCount;
  /**
   * @var int
   */
  public $timeColumnId;
  /**
   * @var string
   */
  public $validRowCount;

  /**
   * @param XPSColumnSpec[]
   */
  public function setColumnSpecs($columnSpecs)
  {
    $this->columnSpecs = $columnSpecs;
  }
  /**
   * @return XPSColumnSpec[]
   */
  public function getColumnSpecs()
  {
    return $this->columnSpecs;
  }
  /**
   * @param string
   */
  public function setImportedDataSizeInBytes($importedDataSizeInBytes)
  {
    $this->importedDataSizeInBytes = $importedDataSizeInBytes;
  }
  /**
   * @return string
   */
  public function getImportedDataSizeInBytes()
  {
    return $this->importedDataSizeInBytes;
  }
  /**
   * @param string
   */
  public function setRowCount($rowCount)
  {
    $this->rowCount = $rowCount;
  }
  /**
   * @return string
   */
  public function getRowCount()
  {
    return $this->rowCount;
  }
  /**
   * @param int
   */
  public function setTimeColumnId($timeColumnId)
  {
    $this->timeColumnId = $timeColumnId;
  }
  /**
   * @return int
   */
  public function getTimeColumnId()
  {
    return $this->timeColumnId;
  }
  /**
   * @param string
   */
  public function setValidRowCount($validRowCount)
  {
    $this->validRowCount = $validRowCount;
  }
  /**
   * @return string
   */
  public function getValidRowCount()
  {
    return $this->validRowCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTableSpec::class, 'Google_Service_CloudNaturalLanguage_XPSTableSpec');
