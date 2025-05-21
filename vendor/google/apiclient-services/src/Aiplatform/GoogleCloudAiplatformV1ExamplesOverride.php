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

class GoogleCloudAiplatformV1ExamplesOverride extends \Google\Collection
{
  protected $collection_key = 'restrictions';
  /**
   * @var int
   */
  public $crowdingCount;
  /**
   * @var string
   */
  public $dataFormat;
  /**
   * @var int
   */
  public $neighborCount;
  protected $restrictionsType = GoogleCloudAiplatformV1ExamplesRestrictionsNamespace::class;
  protected $restrictionsDataType = 'array';
  /**
   * @var bool
   */
  public $returnEmbeddings;

  /**
   * @param int
   */
  public function setCrowdingCount($crowdingCount)
  {
    $this->crowdingCount = $crowdingCount;
  }
  /**
   * @return int
   */
  public function getCrowdingCount()
  {
    return $this->crowdingCount;
  }
  /**
   * @param string
   */
  public function setDataFormat($dataFormat)
  {
    $this->dataFormat = $dataFormat;
  }
  /**
   * @return string
   */
  public function getDataFormat()
  {
    return $this->dataFormat;
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
   * @param GoogleCloudAiplatformV1ExamplesRestrictionsNamespace[]
   */
  public function setRestrictions($restrictions)
  {
    $this->restrictions = $restrictions;
  }
  /**
   * @return GoogleCloudAiplatformV1ExamplesRestrictionsNamespace[]
   */
  public function getRestrictions()
  {
    return $this->restrictions;
  }
  /**
   * @param bool
   */
  public function setReturnEmbeddings($returnEmbeddings)
  {
    $this->returnEmbeddings = $returnEmbeddings;
  }
  /**
   * @return bool
   */
  public function getReturnEmbeddings()
  {
    return $this->returnEmbeddings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExamplesOverride::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExamplesOverride');
