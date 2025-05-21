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

namespace Google\Service\ManufacturerCenter;

class Certification extends \Google\Model
{
  /**
   * @var string
   */
  public $authority;
  /**
   * @var string
   */
  public $code;
  /**
   * @var string
   */
  public $link;
  /**
   * @var string
   */
  public $logo;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $validUntil;
  /**
   * @var string
   */
  public $value;

  /**
   * @param string
   */
  public function setAuthority($authority)
  {
    $this->authority = $authority;
  }
  /**
   * @return string
   */
  public function getAuthority()
  {
    return $this->authority;
  }
  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param string
   */
  public function setLink($link)
  {
    $this->link = $link;
  }
  /**
   * @return string
   */
  public function getLink()
  {
    return $this->link;
  }
  /**
   * @param string
   */
  public function setLogo($logo)
  {
    $this->logo = $logo;
  }
  /**
   * @return string
   */
  public function getLogo()
  {
    return $this->logo;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setValidUntil($validUntil)
  {
    $this->validUntil = $validUntil;
  }
  /**
   * @return string
   */
  public function getValidUntil()
  {
    return $this->validUntil;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Certification::class, 'Google_Service_ManufacturerCenter_Certification');
