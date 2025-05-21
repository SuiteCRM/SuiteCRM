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

class MlStatistics extends \Google\Collection
{
  protected $collection_key = 'iterationResults';
  protected $hparamTrialsType = HparamTuningTrial::class;
  protected $hparamTrialsDataType = 'array';
  protected $iterationResultsType = IterationResult::class;
  protected $iterationResultsDataType = 'array';
  /**
   * @var string
   */
  public $maxIterations;
  /**
   * @var string
   */
  public $modelType;
  /**
   * @var string
   */
  public $trainingType;

  /**
   * @param HparamTuningTrial[]
   */
  public function setHparamTrials($hparamTrials)
  {
    $this->hparamTrials = $hparamTrials;
  }
  /**
   * @return HparamTuningTrial[]
   */
  public function getHparamTrials()
  {
    return $this->hparamTrials;
  }
  /**
   * @param IterationResult[]
   */
  public function setIterationResults($iterationResults)
  {
    $this->iterationResults = $iterationResults;
  }
  /**
   * @return IterationResult[]
   */
  public function getIterationResults()
  {
    return $this->iterationResults;
  }
  /**
   * @param string
   */
  public function setMaxIterations($maxIterations)
  {
    $this->maxIterations = $maxIterations;
  }
  /**
   * @return string
   */
  public function getMaxIterations()
  {
    return $this->maxIterations;
  }
  /**
   * @param string
   */
  public function setModelType($modelType)
  {
    $this->modelType = $modelType;
  }
  /**
   * @return string
   */
  public function getModelType()
  {
    return $this->modelType;
  }
  /**
   * @param string
   */
  public function setTrainingType($trainingType)
  {
    $this->trainingType = $trainingType;
  }
  /**
   * @return string
   */
  public function getTrainingType()
  {
    return $this->trainingType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MlStatistics::class, 'Google_Service_Bigquery_MlStatistics');
