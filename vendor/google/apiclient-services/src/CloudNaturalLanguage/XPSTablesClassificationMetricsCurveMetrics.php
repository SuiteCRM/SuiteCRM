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

class XPSTablesClassificationMetricsCurveMetrics extends \Google\Collection
{
  protected $collection_key = 'confidenceMetricsEntries';
  public $aucPr;
  public $aucRoc;
  protected $confidenceMetricsEntriesType = XPSTablesConfidenceMetricsEntry::class;
  protected $confidenceMetricsEntriesDataType = 'array';
  public $logLoss;
  /**
   * @var int
   */
  public $positionThreshold;
  /**
   * @var string
   */
  public $value;

  public function setAucPr($aucPr)
  {
    $this->aucPr = $aucPr;
  }
  public function getAucPr()
  {
    return $this->aucPr;
  }
  public function setAucRoc($aucRoc)
  {
    $this->aucRoc = $aucRoc;
  }
  public function getAucRoc()
  {
    return $this->aucRoc;
  }
  /**
   * @param XPSTablesConfidenceMetricsEntry[]
   */
  public function setConfidenceMetricsEntries($confidenceMetricsEntries)
  {
    $this->confidenceMetricsEntries = $confidenceMetricsEntries;
  }
  /**
   * @return XPSTablesConfidenceMetricsEntry[]
   */
  public function getConfidenceMetricsEntries()
  {
    return $this->confidenceMetricsEntries;
  }
  public function setLogLoss($logLoss)
  {
    $this->logLoss = $logLoss;
  }
  public function getLogLoss()
  {
    return $this->logLoss;
  }
  /**
   * @param int
   */
  public function setPositionThreshold($positionThreshold)
  {
    $this->positionThreshold = $positionThreshold;
  }
  /**
   * @return int
   */
  public function getPositionThreshold()
  {
    return $this->positionThreshold;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTablesClassificationMetricsCurveMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSTablesClassificationMetricsCurveMetrics');
