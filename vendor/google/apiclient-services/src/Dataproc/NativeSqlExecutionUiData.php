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

namespace Google\Service\Dataproc;

class NativeSqlExecutionUiData extends \Google\Collection
{
  protected $collection_key = 'fallbackNodeToReason';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $executionId;
  /**
   * @var string
   */
  public $fallbackDescription;
  protected $fallbackNodeToReasonType = FallbackReason::class;
  protected $fallbackNodeToReasonDataType = 'array';
  /**
   * @var int
   */
  public $numFallbackNodes;
  /**
   * @var int
   */
  public $numNativeNodes;

  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setExecutionId($executionId)
  {
    $this->executionId = $executionId;
  }
  /**
   * @return string
   */
  public function getExecutionId()
  {
    return $this->executionId;
  }
  /**
   * @param string
   */
  public function setFallbackDescription($fallbackDescription)
  {
    $this->fallbackDescription = $fallbackDescription;
  }
  /**
   * @return string
   */
  public function getFallbackDescription()
  {
    return $this->fallbackDescription;
  }
  /**
   * @param FallbackReason[]
   */
  public function setFallbackNodeToReason($fallbackNodeToReason)
  {
    $this->fallbackNodeToReason = $fallbackNodeToReason;
  }
  /**
   * @return FallbackReason[]
   */
  public function getFallbackNodeToReason()
  {
    return $this->fallbackNodeToReason;
  }
  /**
   * @param int
   */
  public function setNumFallbackNodes($numFallbackNodes)
  {
    $this->numFallbackNodes = $numFallbackNodes;
  }
  /**
   * @return int
   */
  public function getNumFallbackNodes()
  {
    return $this->numFallbackNodes;
  }
  /**
   * @param int
   */
  public function setNumNativeNodes($numNativeNodes)
  {
    $this->numNativeNodes = $numNativeNodes;
  }
  /**
   * @return int
   */
  public function getNumNativeNodes()
  {
    return $this->numNativeNodes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NativeSqlExecutionUiData::class, 'Google_Service_Dataproc_NativeSqlExecutionUiData');
