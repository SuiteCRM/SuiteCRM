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

namespace Google\Service\WorkloadManager;

class Execution extends \Google\Collection
{
  protected $collection_key = 'ruleResults';
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $evaluationId;
  protected $externalDataSourcesType = ExternalDataSources::class;
  protected $externalDataSourcesDataType = 'array';
  /**
   * @var string
   */
  public $inventoryTime;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $noticesType = Notice::class;
  protected $noticesDataType = 'array';
  protected $resultSummaryType = Summary::class;
  protected $resultSummaryDataType = '';
  protected $ruleResultsType = RuleExecutionResult::class;
  protected $ruleResultsDataType = 'array';
  /**
   * @var string
   */
  public $runType;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param string
   */
  public function setEvaluationId($evaluationId)
  {
    $this->evaluationId = $evaluationId;
  }
  /**
   * @return string
   */
  public function getEvaluationId()
  {
    return $this->evaluationId;
  }
  /**
   * @param ExternalDataSources[]
   */
  public function setExternalDataSources($externalDataSources)
  {
    $this->externalDataSources = $externalDataSources;
  }
  /**
   * @return ExternalDataSources[]
   */
  public function getExternalDataSources()
  {
    return $this->externalDataSources;
  }
  /**
   * @param string
   */
  public function setInventoryTime($inventoryTime)
  {
    $this->inventoryTime = $inventoryTime;
  }
  /**
   * @return string
   */
  public function getInventoryTime()
  {
    return $this->inventoryTime;
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
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param Notice[]
   */
  public function setNotices($notices)
  {
    $this->notices = $notices;
  }
  /**
   * @return Notice[]
   */
  public function getNotices()
  {
    return $this->notices;
  }
  /**
   * @param Summary
   */
  public function setResultSummary(Summary $resultSummary)
  {
    $this->resultSummary = $resultSummary;
  }
  /**
   * @return Summary
   */
  public function getResultSummary()
  {
    return $this->resultSummary;
  }
  /**
   * @param RuleExecutionResult[]
   */
  public function setRuleResults($ruleResults)
  {
    $this->ruleResults = $ruleResults;
  }
  /**
   * @return RuleExecutionResult[]
   */
  public function getRuleResults()
  {
    return $this->ruleResults;
  }
  /**
   * @param string
   */
  public function setRunType($runType)
  {
    $this->runType = $runType;
  }
  /**
   * @return string
   */
  public function getRunType()
  {
    return $this->runType;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Execution::class, 'Google_Service_WorkloadManager_Execution');
