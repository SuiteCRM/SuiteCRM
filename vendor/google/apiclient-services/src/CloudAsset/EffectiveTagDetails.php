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

class EffectiveTagDetails extends \Google\Collection
{
  protected $collection_key = 'effectiveTags';
  /**
   * @var string
   */
  public $attachedResource;
  protected $effectiveTagsType = Tag::class;
  protected $effectiveTagsDataType = 'array';

  /**
   * @param string
   */
  public function setAttachedResource($attachedResource)
  {
    $this->attachedResource = $attachedResource;
  }
  /**
   * @return string
   */
  public function getAttachedResource()
  {
    return $this->attachedResource;
  }
  /**
   * @param Tag[]
   */
  public function setEffectiveTags($effectiveTags)
  {
    $this->effectiveTags = $effectiveTags;
  }
  /**
   * @return Tag[]
   */
  public function getEffectiveTags()
  {
    return $this->effectiveTags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EffectiveTagDetails::class, 'Google_Service_CloudAsset_EffectiveTagDetails');
