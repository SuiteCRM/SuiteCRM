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

class GoogleCloudAiplatformV1IndexStats extends \Google\Model
{
  /**
   * @var int
   */
  public $shardsCount;
  /**
   * @var string
   */
  public $sparseVectorsCount;
  /**
   * @var string
   */
  public $vectorsCount;

  /**
   * @param int
   */
  public function setShardsCount($shardsCount)
  {
    $this->shardsCount = $shardsCount;
  }
  /**
   * @return int
   */
  public function getShardsCount()
  {
    return $this->shardsCount;
  }
  /**
   * @param string
   */
  public function setSparseVectorsCount($sparseVectorsCount)
  {
    $this->sparseVectorsCount = $sparseVectorsCount;
  }
  /**
   * @return string
   */
  public function getSparseVectorsCount()
  {
    return $this->sparseVectorsCount;
  }
  /**
   * @param string
   */
  public function setVectorsCount($vectorsCount)
  {
    $this->vectorsCount = $vectorsCount;
  }
  /**
   * @return string
   */
  public function getVectorsCount()
  {
    return $this->vectorsCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1IndexStats::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1IndexStats');
