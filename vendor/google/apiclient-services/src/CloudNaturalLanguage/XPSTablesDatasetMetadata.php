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

class XPSTablesDatasetMetadata extends \Google\Model
{
  /**
   * @var int
   */
  public $mlUseColumnId;
  protected $primaryTableSpecType = XPSTableSpec::class;
  protected $primaryTableSpecDataType = '';
  protected $targetColumnCorrelationsType = XPSCorrelationStats::class;
  protected $targetColumnCorrelationsDataType = 'map';
  /**
   * @var int
   */
  public $targetColumnId;
  /**
   * @var int
   */
  public $weightColumnId;

  /**
   * @param int
   */
  public function setMlUseColumnId($mlUseColumnId)
  {
    $this->mlUseColumnId = $mlUseColumnId;
  }
  /**
   * @return int
   */
  public function getMlUseColumnId()
  {
    return $this->mlUseColumnId;
  }
  /**
   * @param XPSTableSpec
   */
  public function setPrimaryTableSpec(XPSTableSpec $primaryTableSpec)
  {
    $this->primaryTableSpec = $primaryTableSpec;
  }
  /**
   * @return XPSTableSpec
   */
  public function getPrimaryTableSpec()
  {
    return $this->primaryTableSpec;
  }
  /**
   * @param XPSCorrelationStats[]
   */
  public function setTargetColumnCorrelations($targetColumnCorrelations)
  {
    $this->targetColumnCorrelations = $targetColumnCorrelations;
  }
  /**
   * @return XPSCorrelationStats[]
   */
  public function getTargetColumnCorrelations()
  {
    return $this->targetColumnCorrelations;
  }
  /**
   * @param int
   */
  public function setTargetColumnId($targetColumnId)
  {
    $this->targetColumnId = $targetColumnId;
  }
  /**
   * @return int
   */
  public function getTargetColumnId()
  {
    return $this->targetColumnId;
  }
  /**
   * @param int
   */
  public function setWeightColumnId($weightColumnId)
  {
    $this->weightColumnId = $weightColumnId;
  }
  /**
   * @return int
   */
  public function getWeightColumnId()
  {
    return $this->weightColumnId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTablesDatasetMetadata::class, 'Google_Service_CloudNaturalLanguage_XPSTablesDatasetMetadata');
