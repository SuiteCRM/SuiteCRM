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

namespace Google\Service\GKEOnPrem;

class BareMetalStorageConfig extends \Google\Model
{
  protected $lvpNodeMountsConfigType = BareMetalLvpConfig::class;
  protected $lvpNodeMountsConfigDataType = '';
  protected $lvpShareConfigType = BareMetalLvpShareConfig::class;
  protected $lvpShareConfigDataType = '';

  /**
   * @param BareMetalLvpConfig
   */
  public function setLvpNodeMountsConfig(BareMetalLvpConfig $lvpNodeMountsConfig)
  {
    $this->lvpNodeMountsConfig = $lvpNodeMountsConfig;
  }
  /**
   * @return BareMetalLvpConfig
   */
  public function getLvpNodeMountsConfig()
  {
    return $this->lvpNodeMountsConfig;
  }
  /**
   * @param BareMetalLvpShareConfig
   */
  public function setLvpShareConfig(BareMetalLvpShareConfig $lvpShareConfig)
  {
    $this->lvpShareConfig = $lvpShareConfig;
  }
  /**
   * @return BareMetalLvpShareConfig
   */
  public function getLvpShareConfig()
  {
    return $this->lvpShareConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalStorageConfig::class, 'Google_Service_GKEOnPrem_BareMetalStorageConfig');
