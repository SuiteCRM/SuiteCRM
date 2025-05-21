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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaQualityMetrics extends \Google\Model
{
  protected $docNdcgType = GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics::class;
  protected $docNdcgDataType = '';
  protected $docPrecisionType = GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics::class;
  protected $docPrecisionDataType = '';
  protected $docRecallType = GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics::class;
  protected $docRecallDataType = '';
  protected $pageNdcgType = GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics::class;
  protected $pageNdcgDataType = '';
  protected $pageRecallType = GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics::class;
  protected $pageRecallDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function setDocNdcg(GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics $docNdcg)
  {
    $this->docNdcg = $docNdcg;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function getDocNdcg()
  {
    return $this->docNdcg;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function setDocPrecision(GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics $docPrecision)
  {
    $this->docPrecision = $docPrecision;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function getDocPrecision()
  {
    return $this->docPrecision;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function setDocRecall(GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics $docRecall)
  {
    $this->docRecall = $docRecall;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function getDocRecall()
  {
    return $this->docRecall;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function setPageNdcg(GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics $pageNdcg)
  {
    $this->pageNdcg = $pageNdcg;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function getPageNdcg()
  {
    return $this->pageNdcg;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function setPageRecall(GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics $pageRecall)
  {
    $this->pageRecall = $pageRecall;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaQualityMetricsTopkMetrics
   */
  public function getPageRecall()
  {
    return $this->pageRecall;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaQualityMetrics::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaQualityMetrics');
