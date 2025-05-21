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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig extends \Google\Collection
{
  protected $collection_key = 'groupColumns';
  /**
   * @var string[]
   */
  public $groupColumns;
  public $groupTemporalTotalWeight;
  public $groupTotalWeight;
  public $temporalTotalWeight;

  /**
   * @param string[]
   */
  public function setGroupColumns($groupColumns)
  {
    $this->groupColumns = $groupColumns;
  }
  /**
   * @return string[]
   */
  public function getGroupColumns()
  {
    return $this->groupColumns;
  }
  public function setGroupTemporalTotalWeight($groupTemporalTotalWeight)
  {
    $this->groupTemporalTotalWeight = $groupTemporalTotalWeight;
  }
  public function getGroupTemporalTotalWeight()
  {
    return $this->groupTemporalTotalWeight;
  }
  public function setGroupTotalWeight($groupTotalWeight)
  {
    $this->groupTotalWeight = $groupTotalWeight;
  }
  public function getGroupTotalWeight()
  {
    return $this->groupTotalWeight;
  }
  public function setTemporalTotalWeight($temporalTotalWeight)
  {
    $this->temporalTotalWeight = $temporalTotalWeight;
  }
  public function getTemporalTotalWeight()
  {
    return $this->temporalTotalWeight;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig');
