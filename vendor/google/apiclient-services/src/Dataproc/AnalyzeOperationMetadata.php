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

namespace Google\Service\Dataproc;

class AnalyzeOperationMetadata extends \Google\Collection
{
  protected $collection_key = 'warnings';
  /**
   * @var string
   */
  public $analyzedWorkloadName;
  /**
   * @var string
   */
  public $analyzedWorkloadType;
  /**
   * @var string
   */
  public $analyzedWorkloadUuid;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $doneTime;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string[]
   */
  public $warnings;

  /**
   * @param string
   */
  public function setAnalyzedWorkloadName($analyzedWorkloadName)
  {
    $this->analyzedWorkloadName = $analyzedWorkloadName;
  }
  /**
   * @return string
   */
  public function getAnalyzedWorkloadName()
  {
    return $this->analyzedWorkloadName;
  }
  /**
   * @param string
   */
  public function setAnalyzedWorkloadType($analyzedWorkloadType)
  {
    $this->analyzedWorkloadType = $analyzedWorkloadType;
  }
  /**
   * @return string
   */
  public function getAnalyzedWorkloadType()
  {
    return $this->analyzedWorkloadType;
  }
  /**
   * @param string
   */
  public function setAnalyzedWorkloadUuid($analyzedWorkloadUuid)
  {
    $this->analyzedWorkloadUuid = $analyzedWorkloadUuid;
  }
  /**
   * @return string
   */
  public function getAnalyzedWorkloadUuid()
  {
    return $this->analyzedWorkloadUuid;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDoneTime($doneTime)
  {
    $this->doneTime = $doneTime;
  }
  /**
   * @return string
   */
  public function getDoneTime()
  {
    return $this->doneTime;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string[]
   */
  public function setWarnings($warnings)
  {
    $this->warnings = $warnings;
  }
  /**
   * @return string[]
   */
  public function getWarnings()
  {
    return $this->warnings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnalyzeOperationMetadata::class, 'Google_Service_Dataproc_AnalyzeOperationMetadata');
