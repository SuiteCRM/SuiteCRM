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

class XPSXpsOperationMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $exampleCount;
  protected $reportingMetricsType = XPSReportingMetrics::class;
  protected $reportingMetricsDataType = '';
  protected $tablesTrainingOperationMetadataType = XPSTablesTrainingOperationMetadata::class;
  protected $tablesTrainingOperationMetadataDataType = '';
  protected $videoBatchPredictOperationMetadataType = XPSVideoBatchPredictOperationMetadata::class;
  protected $videoBatchPredictOperationMetadataDataType = '';
  protected $videoTrainingOperationMetadataType = XPSVideoTrainingOperationMetadata::class;
  protected $videoTrainingOperationMetadataDataType = '';
  protected $visionTrainingOperationMetadataType = XPSVisionTrainingOperationMetadata::class;
  protected $visionTrainingOperationMetadataDataType = '';

  /**
   * @param string
   */
  public function setExampleCount($exampleCount)
  {
    $this->exampleCount = $exampleCount;
  }
  /**
   * @return string
   */
  public function getExampleCount()
  {
    return $this->exampleCount;
  }
  /**
   * @param XPSReportingMetrics
   */
  public function setReportingMetrics(XPSReportingMetrics $reportingMetrics)
  {
    $this->reportingMetrics = $reportingMetrics;
  }
  /**
   * @return XPSReportingMetrics
   */
  public function getReportingMetrics()
  {
    return $this->reportingMetrics;
  }
  /**
   * @param XPSTablesTrainingOperationMetadata
   */
  public function setTablesTrainingOperationMetadata(XPSTablesTrainingOperationMetadata $tablesTrainingOperationMetadata)
  {
    $this->tablesTrainingOperationMetadata = $tablesTrainingOperationMetadata;
  }
  /**
   * @return XPSTablesTrainingOperationMetadata
   */
  public function getTablesTrainingOperationMetadata()
  {
    return $this->tablesTrainingOperationMetadata;
  }
  /**
   * @param XPSVideoBatchPredictOperationMetadata
   */
  public function setVideoBatchPredictOperationMetadata(XPSVideoBatchPredictOperationMetadata $videoBatchPredictOperationMetadata)
  {
    $this->videoBatchPredictOperationMetadata = $videoBatchPredictOperationMetadata;
  }
  /**
   * @return XPSVideoBatchPredictOperationMetadata
   */
  public function getVideoBatchPredictOperationMetadata()
  {
    return $this->videoBatchPredictOperationMetadata;
  }
  /**
   * @param XPSVideoTrainingOperationMetadata
   */
  public function setVideoTrainingOperationMetadata(XPSVideoTrainingOperationMetadata $videoTrainingOperationMetadata)
  {
    $this->videoTrainingOperationMetadata = $videoTrainingOperationMetadata;
  }
  /**
   * @return XPSVideoTrainingOperationMetadata
   */
  public function getVideoTrainingOperationMetadata()
  {
    return $this->videoTrainingOperationMetadata;
  }
  /**
   * @param XPSVisionTrainingOperationMetadata
   */
  public function setVisionTrainingOperationMetadata(XPSVisionTrainingOperationMetadata $visionTrainingOperationMetadata)
  {
    $this->visionTrainingOperationMetadata = $visionTrainingOperationMetadata;
  }
  /**
   * @return XPSVisionTrainingOperationMetadata
   */
  public function getVisionTrainingOperationMetadata()
  {
    return $this->visionTrainingOperationMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSXpsOperationMetadata::class, 'Google_Service_CloudNaturalLanguage_XPSXpsOperationMetadata');
