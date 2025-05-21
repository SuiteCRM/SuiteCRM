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

namespace Google\Service\CloudAsset;

class Tag extends \Google\Model
{
  /**
   * @var string
   */
  public $tagKey;
  /**
   * @var string
   */
  public $tagKeyId;
  /**
   * @var string
   */
  public $tagValue;
  /**
   * @var string
   */
  public $tagValueId;

  /**
   * @param string
   */
  public function setTagKey($tagKey)
  {
    $this->tagKey = $tagKey;
  }
  /**
   * @return string
   */
  public function getTagKey()
  {
    return $this->tagKey;
  }
  /**
   * @param string
   */
  public function setTagKeyId($tagKeyId)
  {
    $this->tagKeyId = $tagKeyId;
  }
  /**
   * @return string
   */
  public function getTagKeyId()
  {
    return $this->tagKeyId;
  }
  /**
   * @param string
   */
  public function setTagValue($tagValue)
  {
    $this->tagValue = $tagValue;
  }
  /**
   * @return string
   */
  public function getTagValue()
  {
    return $this->tagValue;
  }
  /**
   * @param string
   */
  public function setTagValueId($tagValueId)
  {
    $this->tagValueId = $tagValueId;
  }
  /**
   * @return string
   */
  public function getTagValueId()
  {
    return $this->tagValueId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Tag::class, 'Google_Service_CloudAsset_Tag');
