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

class XPSTablesTrainingOperationMetadata extends \Google\Collection
{
  protected $collection_key = 'trainingObjectivePoints';
  /**
   * @var string
   */
  public $createModelStage;
  /**
   * @var string
   */
  public $optimizationObjective;
  protected $topTrialsType = XPSTuningTrial::class;
  protected $topTrialsDataType = 'array';
  /**
   * @var string
   */
  public $trainBudgetMilliNodeHours;
  protected $trainingObjectivePointsType = XPSTrainingObjectivePoint::class;
  protected $trainingObjectivePointsDataType = 'array';
  /**
   * @var string
   */
  public $trainingStartTime;

  /**
   * @param string
   */
  public function setCreateModelStage($createModelStage)
  {
    $this->createModelStage = $createModelStage;
  }
  /**
   * @return string
   */
  public function getCreateModelStage()
  {
    return $this->createModelStage;
  }
  /**
   * @param string
   */
  public function setOptimizationObjective($optimizationObjective)
  {
    $this->optimizationObjective = $optimizationObjective;
  }
  /**
   * @return string
   */
  public function getOptimizationObjective()
  {
    return $this->optimizationObjective;
  }
  /**
   * @param XPSTuningTrial[]
   */
  public function setTopTrials($topTrials)
  {
    $this->topTrials = $topTrials;
  }
  /**
   * @return XPSTuningTrial[]
   */
  public function getTopTrials()
  {
    return $this->topTrials;
  }
  /**
   * @param string
   */
  public function setTrainBudgetMilliNodeHours($trainBudgetMilliNodeHours)
  {
    $this->trainBudgetMilliNodeHours = $trainBudgetMilliNodeHours;
  }
  /**
   * @return string
   */
  public function getTrainBudgetMilliNodeHours()
  {
    return $this->trainBudgetMilliNodeHours;
  }
  /**
   * @param XPSTrainingObjectivePoint[]
   */
  public function setTrainingObjectivePoints($trainingObjectivePoints)
  {
    $this->trainingObjectivePoints = $trainingObjectivePoints;
  }
  /**
   * @return XPSTrainingObjectivePoint[]
   */
  public function getTrainingObjectivePoints()
  {
    return $this->trainingObjectivePoints;
  }
  /**
   * @param string
   */
  public function setTrainingStartTime($trainingStartTime)
  {
    $this->trainingStartTime = $trainingStartTime;
  }
  /**
   * @return string
   */
  public function getTrainingStartTime()
  {
    return $this->trainingStartTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTablesTrainingOperationMetadata::class, 'Google_Service_CloudNaturalLanguage_XPSTablesTrainingOperationMetadata');
