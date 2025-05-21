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

class XPSTextExtractionEvaluationMetrics extends \Google\Collection
{
  protected $collection_key = 'confidenceMetricsEntries';
  protected $bestF1ConfidenceMetricsType = XPSConfidenceMetricsEntry::class;
  protected $bestF1ConfidenceMetricsDataType = '';
  protected $confidenceMetricsEntriesType = XPSConfidenceMetricsEntry::class;
  protected $confidenceMetricsEntriesDataType = 'array';
  protected $confusionMatrixType = XPSConfusionMatrix::class;
  protected $confusionMatrixDataType = '';
  protected $perLabelConfidenceMetricsType = XPSConfidenceMetricsEntry::class;
  protected $perLabelConfidenceMetricsDataType = 'map';

  /**
   * @param XPSConfidenceMetricsEntry
   */
  public function setBestF1ConfidenceMetrics(XPSConfidenceMetricsEntry $bestF1ConfidenceMetrics)
  {
    $this->bestF1ConfidenceMetrics = $bestF1ConfidenceMetrics;
  }
  /**
   * @return XPSConfidenceMetricsEntry
   */
  public function getBestF1ConfidenceMetrics()
  {
    return $this->bestF1ConfidenceMetrics;
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
   * @param XPSConfidenceMetricsEntry[]
   */
  public function setPerLabelConfidenceMetrics($perLabelConfidenceMetrics)
  {
    $this->perLabelConfidenceMetrics = $perLabelConfidenceMetrics;
  }
  /**
   * @return XPSConfidenceMetricsEntry[]
   */
  public function getPerLabelConfidenceMetrics()
  {
    return $this->perLabelConfidenceMetrics;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTextExtractionEvaluationMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSTextExtractionEvaluationMetrics');
