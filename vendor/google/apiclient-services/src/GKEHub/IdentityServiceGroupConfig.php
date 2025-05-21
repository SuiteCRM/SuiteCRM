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

namespace Google\Service\GKEHub;

class IdentityServiceGroupConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $baseDn;
  /**
   * @var string
   */
  public $filter;
  /**
   * @var string
   */
  public $idAttribute;

  /**
   * @param string
   */
  public function setBaseDn($baseDn)
  {
    $this->baseDn = $baseDn;
  }
  /**
   * @return string
   */
  public function getBaseDn()
  {
    return $this->baseDn;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param string
   */
  public function setIdAttribute($idAttribute)
  {
    $this->idAttribute = $idAttribute;
  }
  /**
   * @return string
   */
  public function getIdAttribute()
  {
    return $this->idAttribute;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IdentityServiceGroupConfig::class, 'Google_Service_GKEHub_IdentityServiceGroupConfig');
