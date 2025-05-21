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

namespace Google\Service\Storage;

class RelocateBucketRequest extends \Google\Model
{
  protected $destinationCustomPlacementConfigType = RelocateBucketRequestDestinationCustomPlacementConfig::class;
  protected $destinationCustomPlacementConfigDataType = '';
  /**
   * @var string
   */
  public $destinationLocation;
  /**
   * @var bool
   */
  public $validateOnly;

  /**
   * @param RelocateBucketRequestDestinationCustomPlacementConfig
   */
  public function setDestinationCustomPlacementConfig(RelocateBucketRequestDestinationCustomPlacementConfig $destinationCustomPlacementConfig)
  {
    $this->destinationCustomPlacementConfig = $destinationCustomPlacementConfig;
  }
  /**
   * @return RelocateBucketRequestDestinationCustomPlacementConfig
   */
  public function getDestinationCustomPlacementConfig()
  {
    return $this->destinationCustomPlacementConfig;
  }
  /**
   * @param string
   */
  public function setDestinationLocation($destinationLocation)
  {
    $this->destinationLocation = $destinationLocation;
  }
  /**
   * @return string
   */
  public function getDestinationLocation()
  {
    return $this->destinationLocation;
  }
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RelocateBucketRequest::class, 'Google_Service_Storage_RelocateBucketRequest');
