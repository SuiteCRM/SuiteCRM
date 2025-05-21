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

namespace Google\Service\Compute;

class SecurityPolicyUserDefinedField extends \Google\Model
{
  /**
   * @var string
   */
  public $base;
  /**
   * @var string
   */
  public $mask;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $offset;
  /**
   * @var int
   */
  public $size;

  /**
   * @param string
   */
  public function setBase($base)
  {
    $this->base = $base;
  }
  /**
   * @return string
   */
  public function getBase()
  {
    return $this->base;
  }
  /**
   * @param string
   */
  public function setMask($mask)
  {
    $this->mask = $mask;
  }
  /**
   * @return string
   */
  public function getMask()
  {
    return $this->mask;
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
   * @param int
   */
  public function setOffset($offset)
  {
    $this->offset = $offset;
  }
  /**
   * @return int
   */
  public function getOffset()
  {
    return $this->offset;
  }
  /**
   * @param int
   */
  public function setSize($size)
  {
    $this->size = $size;
  }
  /**
   * @return int
   */
  public function getSize()
  {
    return $this->size;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SecurityPolicyUserDefinedField::class, 'Google_Service_Compute_SecurityPolicyUserDefinedField');
