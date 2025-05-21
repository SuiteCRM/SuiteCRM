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

namespace Google\Service\AndroidPublisher;

class InappproductsDeleteRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $latencyTolerance;
  /**
   * @var string
   */
  public $packageName;
  /**
   * @var string
   */
  public $sku;

  /**
   * @param string
   */
  public function setLatencyTolerance($latencyTolerance)
  {
    $this->latencyTolerance = $latencyTolerance;
  }
  /**
   * @return string
   */
  public function getLatencyTolerance()
  {
    return $this->latencyTolerance;
  }
  /**
   * @param string
   */
  public function setPackageName($packageName)
  {
    $this->packageName = $packageName;
  }
  /**
   * @return string
   */
  public function getPackageName()
  {
    return $this->packageName;
  }
  /**
   * @param string
   */
  public function setSku($sku)
  {
    $this->sku = $sku;
  }
  /**
   * @return string
   */
  public function getSku()
  {
    return $this->sku;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InappproductsDeleteRequest::class, 'Google_Service_AndroidPublisher_InappproductsDeleteRequest');
