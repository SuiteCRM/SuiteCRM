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

namespace Google\Service\DatabaseMigrationService;

class MultiColumnDatatypeChange extends \Google\Model
{
  /**
   * @var array[]
   */
  public $customFeatures;
  /**
   * @var string
   */
  public $newDataType;
  /**
   * @var int
   */
  public $overrideFractionalSecondsPrecision;
  /**
   * @var string
   */
  public $overrideLength;
  /**
   * @var int
   */
  public $overridePrecision;
  /**
   * @var int
   */
  public $overrideScale;
  /**
   * @var string
   */
  public $sourceDataTypeFilter;
  protected $sourceNumericFilterType = SourceNumericFilter::class;
  protected $sourceNumericFilterDataType = '';
  protected $sourceTextFilterType = SourceTextFilter::class;
  protected $sourceTextFilterDataType = '';

  /**
   * @param array[]
   */
  public function setCustomFeatures($customFeatures)
  {
    $this->customFeatures = $customFeatures;
  }
  /**
   * @return array[]
   */
  public function getCustomFeatures()
  {
    return $this->customFeatures;
  }
  /**
   * @param string
   */
  public function setNewDataType($newDataType)
  {
    $this->newDataType = $newDataType;
  }
  /**
   * @return string
   */
  public function getNewDataType()
  {
    return $this->newDataType;
  }
  /**
   * @param int
   */
  public function setOverrideFractionalSecondsPrecision($overrideFractionalSecondsPrecision)
  {
    $this->overrideFractionalSecondsPrecision = $overrideFractionalSecondsPrecision;
  }
  /**
   * @return int
   */
  public function getOverrideFractionalSecondsPrecision()
  {
    return $this->overrideFractionalSecondsPrecision;
  }
  /**
   * @param string
   */
  public function setOverrideLength($overrideLength)
  {
    $this->overrideLength = $overrideLength;
  }
  /**
   * @return string
   */
  public function getOverrideLength()
  {
    return $this->overrideLength;
  }
  /**
   * @param int
   */
  public function setOverridePrecision($overridePrecision)
  {
    $this->overridePrecision = $overridePrecision;
  }
  /**
   * @return int
   */
  public function getOverridePrecision()
  {
    return $this->overridePrecision;
  }
  /**
   * @param int
   */
  public function setOverrideScale($overrideScale)
  {
    $this->overrideScale = $overrideScale;
  }
  /**
   * @return int
   */
  public function getOverrideScale()
  {
    return $this->overrideScale;
  }
  /**
   * @param string
   */
  public function setSourceDataTypeFilter($sourceDataTypeFilter)
  {
    $this->sourceDataTypeFilter = $sourceDataTypeFilter;
  }
  /**
   * @return string
   */
  public function getSourceDataTypeFilter()
  {
    return $this->sourceDataTypeFilter;
  }
  /**
   * @param SourceNumericFilter
   */
  public function setSourceNumericFilter(SourceNumericFilter $sourceNumericFilter)
  {
    $this->sourceNumericFilter = $sourceNumericFilter;
  }
  /**
   * @return SourceNumericFilter
   */
  public function getSourceNumericFilter()
  {
    return $this->sourceNumericFilter;
  }
  /**
   * @param SourceTextFilter
   */
  public function setSourceTextFilter(SourceTextFilter $sourceTextFilter)
  {
    $this->sourceTextFilter = $sourceTextFilter;
  }
  /**
   * @return SourceTextFilter
   */
  public function getSourceTextFilter()
  {
    return $this->sourceTextFilter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MultiColumnDatatypeChange::class, 'Google_Service_DatabaseMigrationService_MultiColumnDatatypeChange');
