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

class XPSClassificationEvaluationMetrics extends \Google\Collection
{
  protected $collection_key = 'confidenceMetricsEntries';
  /**
   * @var float
   */
  public $auPrc;
  /**
   * @var float
   */
  public $auRoc;
  /**
   * @var float
   */
  public $baseAuPrc;
  protected $confidenceMetricsEntriesType = XPSConfidenceMetricsEntry::class;
  protected $confidenceMetricsEntriesDataType = 'array';
  protected $confusionMatrixType = XPSConfusionMatrix::class;
  protected $confusionMatrixDataType = '';
  /**
   * @var int
   */
  public $evaluatedExamplesCount;
  /**
   * @var float
   */
  public $logLoss;

  /**
   * @param float
   */
  public function setAuPrc($auPrc)
  {
    $this->auPrc = $auPrc;
  }
  /**
   * @return float
   */
  public function getAuPrc()
  {
    return $this->auPrc;
  }
  /**
   * @param float
   */
  public function setAuRoc($auRoc)
  {
    $this->auRoc = $auRoc;
  }
  /**
   * @return float
   */
  public function getAuRoc()
  {
    return $this->auRoc;
  }
  /**
   * @param float
   */
  public function setBaseAuPrc($baseAuPrc)
  {
    $this->baseAuPrc = $baseAuPrc;
  }
  /**
   * @return float
   */
  public function getBaseAuPrc()
  {
    return $this->baseAuPrc;
  }
  /**
   * @param XPSConfidenceMetricsEntry[]
   */
  public function setConfidenceMetricsEntries($confidenceMetricsEntries)
  {
    $this->confidenceMetricsEntries = $confidenceMetricsEntries;
  }
  /**
   * @return XPSConfidenceMetricsEntry[]
   */
  public function getConfidenceMetricsEntries()
  {
    return $this->confidenceMetricsEntries;
  }
  /**
   * @param XPSConfusionMatrix
   */
  public function setConfusionMatrix(XPSConfusionMatrix $confusionMatrix)
  {
    $this->confusionMatrix = $confusionMatrix;
  }
  /**
   * @return XPSConfusionMatrix
   */
  public function getConfusionMatrix()
  {
    return $this->confusionMatrix;
  }
  /**
   * @param int
   */
  public function setEvaluatedExamplesCount($evaluatedExamplesCount)
  {
    $this->evaluatedExamplesCount = $evaluatedExamplesCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedExamplesCount()
  {
    return $this->evaluatedExamplesCount;
  }
  /**
   * @param float
   */
  public function setLogLoss($logLoss)
  {
    $this->logLoss = $logLoss;
  }
  /**
   * @return float
   */
  public function getLogLoss()
  {
    return $this->logLoss;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSClassificationEvaluationMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSClassificationEvaluationMetrics');
