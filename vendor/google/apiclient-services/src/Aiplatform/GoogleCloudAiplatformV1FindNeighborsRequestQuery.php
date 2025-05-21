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

class GoogleCloudAiplatformV1FindNeighborsRequestQuery extends \Google\Model
{
  /**
   * @var int
   */
  public $approximateNeighborCount;
  protected $datapointType = GoogleCloudAiplatformV1IndexDatapoint::class;
  protected $datapointDataType = '';
  public $fractionLeafNodesToSearchOverride;
  /**
   * @var int
   */
  public $neighborCount;
  /**
   * @var int
   */
  public $perCrowdingAttributeNeighborCount;
  protected $rrfType = GoogleCloudAiplatformV1FindNeighborsRequestQueryRRF::class;
  protected $rrfDataType = '';

  /**
   * @param int
   */
  public function setApproximateNeighborCount($approximateNeighborCount)
  {
    $this->approximateNeighborCount = $approximateNeighborCount;
  }
  /**
   * @return int
   */
  public function getApproximateNeighborCount()
  {
    return $this->approximateNeighborCount;
  }
  /**
   * @param GoogleCloudAiplatformV1IndexDatapoint
   */
  public function setDatapoint(GoogleCloudAiplatformV1IndexDatapoint $datapoint)
  {
    $this->datapoint = $datapoint;
  }
  /**
   * @return GoogleCloudAiplatformV1IndexDatapoint
   */
  public function getDatapoint()
  {
    return $this->datapoint;
  }
  public function setFractionLeafNodesToSearchOverride($fractionLeafNodesToSearchOverride)
  {
    $this->fractionLeafNodesToSearchOverride = $fractionLeafNodesToSearchOverride;
  }
  public function getFractionLeafNodesToSearchOverride()
  {
    return $this->fractionLeafNodesToSearchOverride;
  }
  /**
   * @param int
   */
  public function setNeighborCount($neighborCount)
  {
    $this->neighborCount = $neighborCount;
  }
  /**
   * @return int
   */
  public function getNeighborCount()
  {
    return $this->neighborCount;
  }
  /**
   * @param int
   */
  public function setPerCrowdingAttributeNeighborCount($perCrowdingAttributeNeighborCount)
  {
    $this->perCrowdingAttributeNeighborCount = $perCrowdingAttributeNeighborCount;
  }
  /**
   * @return int
   */
  public function getPerCrowdingAttributeNeighborCount()
  {
    return $this->perCrowdingAttributeNeighborCount;
  }
  /**
   * @param GoogleCloudAiplatformV1FindNeighborsRequestQueryRRF
   */
  public function setRrf(GoogleCloudAiplatformV1FindNeighborsRequestQueryRRF $rrf)
  {
    $this->rrf = $rrf;
  }
  /**
   * @return GoogleCloudAiplatformV1FindNeighborsRequestQueryRRF
   */
  public function getRrf()
  {
    return $this->rrf;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FindNeighborsRequestQuery::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FindNeighborsRequestQuery');
